module Hw2 where

import Data.List.Split
import Data.Maybe

import qualified Data.Char as C

--HERE BE DRAGONS
--TODO: Remove me
import Debug.Trace

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ i []     = i
myFoldl f i (x:xs) = f (myFoldl f i xs) x

myReverse :: [a] -> [a]
myReverse = myFoldl (\rev x -> rev ++ [x]) []

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ i []     = i
myFoldr f i (x:xs) = f x (myFoldr f i xs)

myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 f = foldr (flip f)

isUpper :: Char -> Bool
isUpper = flip elem ['A'..'Z']

onlyCapitals1 :: String -> String
onlyCapitals1 = filter isUpper

onlyCapitals2 :: String -> String
onlyCapitals2 s = [c | c <- s, isUpper c]

onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (c:cs)
        | isUpper c = c : onlyCapitals3 cs
        | otherwise = onlyCapitals3 cs

divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = (quot', rem')
        where quot' = x `div` y
              rem'  = x `mod` y

digitSum :: Int -> Int
digitSum = sum . map C.digitToInt . show . abs

largeNums :: [(Int, String)]
largeNums = [(3, "thousand"), (6, "million"), (9, "billion"),
             (12, "trillion"), (15, "quadrillion"),
             (18, "quintillion"), (21, "sextillion"),
             (24, "septillion"), (27, "octillion"),
             (30, "nonillion"), (33, "decillion"),
             (36, "undecillion"), (39, "duodecillion"),
             (42, "tredecillion"), (45, "quattuordecillion"),
             (48, "quindecillion"), (51, "sexdecillion"),
             (54, "septendecillion"), (57, "octodecillion"),
             (60, "novemdecillion"), (63, "vigintillion")]
tens :: [String]
tens = ["", "ten", "twenty", "thirty", "forty", "fifty",
        "sixty", "seventy", "eighty", "ninety"]
ones :: [String]
ones = ["", "one", "two", "three", "four", "five", "six",
        "seven", "eight", "nine"]
teens :: [String]
teens = ["", "eleven", "twelve", "thirteen",
         "fourteen", "fifteen", "sixteen",
         "seventeen", "eighteen", "nineteen"]

sayNum :: String -> String
sayNum = (++ " ") . unwords . removeLastZero . words . convNum
        where removeLastZero it = if length it > 1 && last it == "zero"
                                      then init it
                                      else it
              convNum :: String -> String
              convNum [] = []
              convNum (c:cs) =
                  case length cs `mod` 3 of
                      0 | not (null cs) ->
                               ones !! C.digitToInt c ++ " "
                               ++ fromJust (lookup (length cs) largeNums)
                               ++ " " ++ convNum cs
                        | c == '0' -> "zero "
                        | otherwise -> ones !! C.digitToInt c ++ " "
                      1 | c /= '1' -> tens !! C.digitToInt c ++ " " ++ convNum cs
                        | head cs == '0' -> "ten " ++ convNum cs
                        --otherwise => 1[1..9] ie: teens
                        | otherwise -> teens !! C.digitToInt (head cs) ++ " "
                               ++ fromMaybe "" (lookup (length cs - 1) largeNums)
                               ++ " " ++ convNum (tail cs)
                      2 | c /= '0'  -> ones !! C.digitToInt c ++ " hundred "
                               ++ convNum cs
                        | otherwise -> convNum cs
