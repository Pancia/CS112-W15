module Hw2 where

import Data.List.Split

import qualified Data.Char as C

large_nums = [(3, "thousand"), (6, "million"), (9, "billion"), 
              (12, "trillion"), (15, "quadrillion"), 
              (18, "quintillion"), (21, "sextillion"), 
              (24, "septillion"), (27, "octillion"), 
              (30, "nonillion"), (33, "decillion"), 
              (36, "undecillion"), (39, "duodecillion"),
              (42, "tredecillion"), (45, "quattuordecillion"),
              (48, "quindecillion"), (51, "sexdecillion"),
              (54, "septendecillion"), (57, "octodecillion"),
              (60, "novemdecillion"), (63, "vigintillion")]
tens = [("1", "ten"), ("2", "twenty"), ("3", "thirty"), ("4", "forty"),
        ("5", "fifty"), ("6", "sixty"), ("7", "seventy"), ("8", "eighty"),
      	("9", "ninety")]
ones = ["zero", "one", "two", "three", "four", "five", "six", 
        "seven", "eight", "nine"]
teens = [("1", "eleven"), ("2", "twelve"), ("3", "thriteen"),
       	 ("4", "fourteen"), ("5", "fifteen"), ("6", "sixteen"),
       	 ("7", "seventeen"), ("8", "eighteen"), ("9", "nineteen")]

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

sayNum :: String -> String
sayNum (c1:cs) = case ((length cs) `mod` 3) of
					0 -> ones !! ((C.ord(c1)) - 48) ++ " " ++ 
					     (snd (large_nums !! (((length cs) `div` 3) - 1)))
					1 -> "not handled: mod is 1"
					2 -> "not handled: mod is 2"
--if 0 then hundreds
--if 1 then ones
--if 2 then tens
		
