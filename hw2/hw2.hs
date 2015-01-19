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
tens = ["ten", "twenty", "thirty", "forty", "fifty", 
        "sixty", "seventy", "eighty", "ninety"]
ones = ["one", "two", "three", "four", "five", "six", 
        "seven", "eight", "nine"]
teens = ["eleven", "twelve", "thriteen", "fourteen", "fifteen", "sixteen",
       	 "seventeen", "eighteen", "nineteen"]

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
sayNum [] = []
--Not sure how to do case "0". Line below makes weird print statements
--sayNum "0" = "zero"
sayNum (c1:cs) = case ((length cs) `mod` 3) of
					--if 0 then ones
					0 -> if ((length cs) > 0)
							then 
								ones !! ((C.ord(c1)) - 49) ++ " " ++ 
					     (snd (large_nums !! (((length cs) `div` 3) - 1))) ++ 
					     " " ++ (sayNum cs)
					     	else
					     		ones !! ((C.ord(c1)) - 49) ++ " " ++ (sayNum cs)
					--if 1 then tens
					1 -> if (c1 /= '1')
							then 
								tens !! ((C.ord(c1)) - 49) ++ " " ++ (sayNum cs)
							else if ((head cs) == '0')
								then
									"ten " ++ (sayNum cs)
							else
								teens !! ((C.ord(head cs) - 49)) ++ " " ++ 
								(snd (large_nums !! (((length cs) `div` 3) - 1))) ++ 
					     		" " ++ (sayNum (tail cs))
					--if 2 then hundreds
					2 -> if (c1 /= '0')
							then
								ones !! ((C.ord(c1)) - 49) ++ " hundred " ++
								(sayNum cs)
							else
								(sayNum cs)
		
