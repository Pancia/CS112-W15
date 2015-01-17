module Hw2 where

import Data.List.Split

import qualified Data.Char as C

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

--sayNum :: String -> String
sayNum = chunksOf 3 . reverse
