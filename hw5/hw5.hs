module Hw5 where

import Control.Applicative hiding ((<|>), empty)
import Data.Map
import Data.Char (toLower)

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

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | Sequence Statement Statement      -- s1; s2
               | Skip                              -- no-op

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
        -- <|> do
         --    P.reserved lexer "for"
         --    var <- P.identifier lexer
         --    P.reserved lexer "in"
         --    low <- exprParser
         --    P.reserved lexer "to"
         --    up <- exprParser
         --    P.reserved lexer "do"
         --    body <- stmtParser
         --    P.reserved lexer "end"
         --    return (For var low up body)

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

-- Parse and run the given WHILE programs
run :: (Show v) => Parser n -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case parse parser "" input of
    Left err -> print err
    Right x  -> print (eval x empty)

-- Parse and run the given WHILE programs using monads
--runMonad :: String -> Maybe Store
--runMonad input = proc (parse stmtParser "" input)
--    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
--          proc _         = Nothing
