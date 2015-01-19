module Hw2 where

import Data.List.Split
import Data.Maybe

import qualified Data.Char as C

--HERE BE DRAGONS
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
tens = ["", "ten", "twenty", "thirty", "forty", "fifty",
        "sixty", "seventy", "eighty", "ninety"]
ones = ["", "one", "two", "three", "four", "five", "six",
        "seven", "eight", "nine"]
teens = ["", "eleven", "twelve", "thirteen",
         "fourteen", "fifteen", "sixteen",
         "seventeen", "eighteen", "nineteen"]

sayNum :: String -> String
sayNum = (++ " ") . unwords . removeLastZero . words . sayNum'
        where removeLastZero it = if length it > 1 && last it == "zero"
                                      then init it
                                      else it

sayNum' :: String -> String
sayNum' [] = []
sayNum' num@(c:cs) = case length cs `mod` 3 of
                    --if 0 then ones place
                    0 -> if (length cs) > 0
                            --if not the ones place of the original input
                            then
                                ones !! C.digitToInt c ++ " " ++
                                fromJust (lookup (length cs) largeNums) ++
                                " " ++ sayNum cs
                             --if it is the ones place from the original input
                             else if c == '0' then 
                                "zero "
                             else
                                 ones !! C.digitToInt c ++ " "
                    --if 1 then tens place
                    1 -> if c /= '1'
                            --if the number is not 1
                            then
                                tens !! C.digitToInt c ++ " " ++ sayNum cs
                            --if the second number is 0 then it is ten
                            else if head cs == '0'
                                then
                                    "ten " ++ sayNum cs
                            --if the first number is one and the second is not 0
                            --then the number is 11, 12... handles special case
                            else
                                teens !! C.digitToInt (head cs) ++ " " ++
                                fromMaybe "" (lookup (length cs - 1) largeNums) ++
                                " " ++ (sayNum (tail cs))
                    --if 2 then hundreds place
                    2 -> if c /= '0'
                            --if c is not 0 then say something like "three hundred"
                            then
                                ones !! C.digitToInt c ++ " hundred " ++
                                sayNum cs
                            --else, say nothing because you don't say "zero hundred"
                            else
                                sayNum cs

