module Hw5 where

import Control.Applicative hiding ((<|>), empty)
import Data.Map as M
import Data.Char (toLower)
import Data.Maybe

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value
           deriving (Eq)

instance Ord Value where
    (IntVal v1) `compare` (IntVal v2) = v1 `compare` v2
    _ `compare` _ = error "cant compare BoolVals"

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

--store = map var val
evalE :: Expression -> Store -> (Value, Store)
evalE (Assignment x v) s = (v', s'')
    where (v', s') = evalE v s
          s'' = M.insert x v' s'
evalE (BinOp op e1 e2) s = (applyOp op e1' e2', s'')
    where (e1', s')  = evalE e1 s
          (e2', s'') = evalE e2 s'
evalE (Var x) s = (x', s)
    where x' = fromMaybe (error "not found") $ M.lookup x s
evalE (Val v) s = (v, s)

evalE_maybe :: Expression -> Store -> Maybe (Value, Store)
evalE_maybe (Assignment x v) s = do (v', s') <- evalE_maybe v s
                                    return (v', M.insert x v' s')
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Var x) s = do x' <- M.lookup x s
                           return (x', s)
evalE_maybe (Val v) s = Just (v, s)

--Imperative a = Store -> Maybe (a, Store)
evalE_monad :: Expression -> Imperative Value
evalE_monad (BinOp o a b) = do a' <- evalE_monad a
                               b' <- evalE_monad b
                               return (applyOp o a' b')
evalE_monad (Var x) = getVar x
evalE_monad (Val v) = return v
evalE_monad (Assignment x e) = do e' <- evalE_monad e
                                  setVar x e'

getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> (M.lookup var store >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, M.insert var val store))

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | Sequence Statement Statement      -- s1; s2
               | For Variable Expression Expression Statement
               | Skip                              -- no-op

evalS :: Statement -> Store -> Store
evalS w@(While e s1) s = case evalE e s of
                             (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
                             (BoolVal False,s') -> s'
                             _                  -> error "Condition must be a BoolVal"
evalS Skip s             = s
evalS (Expr e) s         = snd $ evalE e s
evalS (Sequence s1 s2) s = evalS s2 $ evalS s1 s
evalS (If e s1 s2) s     = case evalE e s of
                             (BoolVal True,s')  -> evalS s1 s'
                             (BoolVal False,s') -> evalS s2 s'
                             _                  -> error "Condition must be a BoolVal"
evalS f@(For v e1 e2 st) s = undefined

evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe w@(While e s1) s = do x <- evalE_maybe e s
                                  case x of
                                      (BoolVal True,s') -> do s'' <- evalS_maybe s1 s'
                                                              evalS_maybe w s''
                                      (BoolVal False,s') -> return s'
                                      _ -> Nothing
evalS_maybe Skip s = return s
evalS_maybe (Expr e) s = return . snd =<< evalE_maybe e s
evalS_maybe (Sequence s1 s2) s = evalS_maybe s2 =<< evalS_maybe s1 s
evalS_maybe (If e s1 s2) s = do x <- evalE_maybe e s
                                case x of
                                    (BoolVal True,s') -> evalS_maybe s1 s'
                                    (BoolVal False,s') -> evalS_maybe s2 s'
                                    _ -> Nothing

evalS_monad :: Statement -> Imperative ()
evalS_monad (While e s1) = do e' <- evalE_monad e
                              case e' of
                                  BoolVal True -> evalS_monad s1
                                  BoolVal False -> return ()
                                  _             -> error "Condition must be a BoolVal"
evalS_monad Skip = return ()
evalS_monad (Sequence s1 s2) = do evalS_monad s1
                                  evalS_monad s2
                                  return ()
evalS_monad (Expr e) = do evalE_monad e
                          return ()
evalS_monad (If e s1 s2) = do e' <- evalE_monad e
                              case e' of
                                  BoolVal True -> evalS_monad s1
                                  BoolVal False -> evalS_monad s2
                                  _             -> error "Condition must be a BoolVal"
evalS_monad (For v start end body) = do init <- evalE_monad start
                                        final <- evalE_monad end
                                        setVar v (applyOp Plus init $ IntVal 1)
                                        evalS_monad body
                                        v' <- getVar v
                                        if (v' < final)
                                           then evalS_monad (For v (Val v') end body)
                                           else do setVar v $ applyOp Plus v' $ IntVal 1
                                                   return ()


-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

instance Show Value where
        show (IntVal i) = show i
        show (BoolVal b) = toLower <$> show b

instance Show Op where
        show x = case x of
                     Plus -> " + "
                     Minus -> " - "
                     Times -> " * "
                     GreaterThan -> " < "
                     Equals -> " == "
                     LessThan -> " < "

instance Show Expression where
        show (Var x) = x
        show (Val v) = show v
        show (BinOp op e1 e2) = show e1 ++ show op ++ show e2
        show (Assignment v e) = v ++ " = " ++ show e

instance Show Statement where
        show (Expr e) = show e
        show (If e s1 s2) = "if " ++ show e ++ " then " ++ show s1 ++ " else " ++ show s2 ++ " end"
        show (While e s) = "while " ++ show e ++ " do " ++ show s ++ " end"
        show (Sequence s1 s2) = show s1 ++ ";" ++ show s2
        show Skip = "no-op"
        show (For v e1 e2 s) = "for " ++ v ++ " in " ++ show e1 ++ " to " ++ show e2 ++ " do " ++ show s ++ " end"

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = P.reservedOp lexer name >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
          <|> do
              P.reserved lexer "for"
              var <- P.identifier lexer
              P.reserved lexer "in"
              low <- exprParser
              P.reserved lexer "to"
              up <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (For var low up body)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)

p' :: String -> String
p' inp = case parse stmtParser "" inp of
             Right x -> show x
             x -> error . show $ x

-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case parse stmtParser "" input of
    Left err -> print err
    Right x  -> print x

r' :: (Show v) => Parser n -> String -> (n -> Store -> v) -> v
r' parser input eval = case parse parser "" input of
                           Left err -> error $ show err
                           Right x -> eval x empty

-- Parse and run the given WHILE programs
run :: (Show v) => Parser n -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case parse parser "" input of
    Left err -> print err
    Right x  -> print (eval x empty)

newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- runImperative b s
                                   runImperative (f v1) s1)
    fail _ = Imperative $ const Nothing

-- Parse and run the given WHILE programs using monads
runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing
