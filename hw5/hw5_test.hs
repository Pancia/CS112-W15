module Hw5Test where

import Test.HUnit
import Hw5

import Text.Parsec (parse)
import Data.Map
import Data.Either

main :: IO Counts
main = runTestTT $ TestList tests

tests :: [Test]
tests = [p' "1+1" ~?= "1 + 1"
        ,p' "23*x<42" ~?= "23 * x < 42"
        ,p' "if false then x=2 else x = 3 end ; x = x + 2" ~?= "if false then x = 2 else x = 3 end;x = x + 2"
        ,p' "x = 1; while x < 5 do x = x + 1 end" ~?= "x = 1;while x < 5 do x = x + 1 end"
        ,evalE (Val (BoolVal True)) Data.Map.empty ~?= (BoolVal True,fromList [])
        ,r' exprParser "1+1" evalE ~?= (IntVal 2,fromList [])
        ,r' exprParser "13*2 < 27" evalE ~?= (BoolVal True,fromList [])
        ,evalE (Var "x") (fromList [("x",IntVal 23)]) ~?= (IntVal 23,fromList [("x",IntVal 23)])
        ,r' exprParser "x = 23" evalE ~?= (IntVal 23,fromList [("x",IntVal 23)])
        ,r' exprParser "x = y = 2 + 3" evalE ~?= (IntVal 5,fromList [("x",IntVal 5),("y",IntVal 5)])
      --
        ,r' stmtParser "x=1+1" evalS ~?= fromList [("x",IntVal 2)]
        ,r' stmtParser "x = 2; x = x + 3" evalS ~?= fromList [("x",IntVal 5)]
        ,r' stmtParser "if true then x = 1 else x = 2 end" evalS ~?= fromList [("x",IntVal 1)]
        ,r' stmtParser "x = 2; y = x + 3; if y < 4 then z = true else z = false end" evalS ~?= fromList [("x",IntVal 2),("y",IntVal 5),("z",BoolVal False)]
        ,r' stmtParser "x = 1; while x < 3 do x = x + 1 end" evalS ~?= fromList [("x",IntVal 3)]
        ,r' stmtParser "x = 1 ; y = 1; while x < 5 do x = x + 1 ; y = y * x end" evalS ~?= fromList [("x",IntVal 5),("y",IntVal 120)]
      --
        ,r' exprParser "1+1" evalE_maybe ~?= Just (IntVal 2,fromList [])
        ,r' exprParser "10 < x + 1" evalE_maybe ~?= Nothing
        ,r' exprParser "10 == 4 * 2" evalE_maybe ~?= Just (BoolVal False,fromList [])
        ,r' stmtParser "x = 2; y = z" evalS_maybe ~?= Nothing
        ,r' stmtParser "x = true; if x then y = 1 else y = 2 end" evalS_maybe ~?= Just (fromList [("x",BoolVal True),("y",IntVal 1)])
        ,r' stmtParser "x = 1; if x then y = 1 else y = 2 end" evalS_maybe ~?= Nothing
      --
        ,runMonad "x = 1" ~?= Just (fromList [("x", IntVal 1)])
        ,runMonad "x = 1; if x == 1 then y = 1 else y = 2 end" ~?= Just (fromList [("x", IntVal 1),("y", IntVal 1)])
        ,runMonad "x = 1; if x == z then y = 1 else y = 2 end" ~?= Nothing
        ,runMonad "while 23 x = x + 1 end" ~?= Nothing
      --
        ,show (For "a" (Val (IntVal 1)) (Val (IntVal 2)) (Expr (Val (IntVal 3)))) ~?= "for a in 1 to 2 do 3 end"
        ,show (parse stmtParser "" "for x in 1 to 4 do y = x end") ~?= "Right for x in 1 to 4 do y = x end"
        ,r' stmtParser "for x in 1 to 4 do y = x end" evalS ~?= fromList [("x",IntVal 5),("y",IntVal 4)]
        ,r' stmtParser "for x in 1 to 3 do y = x end" evalS_maybe ~?= Just (fromList [("x",IntVal 4),("y",IntVal 3)])
        ,r' stmtParser "for x in 1 to 4 do y = z end" evalS_maybe ~?= Nothing
        ,runMonad "for x in 1 to 4 do y = x end" ~?= Just (fromList [("x",IntVal 5),("y",IntVal 4)])
        ,runMonad "for x in 1 to 4 do y = z end" ~?= Nothing
        ]
