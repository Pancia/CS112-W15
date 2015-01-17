import Test.HUnit
import Hw2

main :: IO Counts
main = runTestTT $ TestList hw2Tests

hw2Tests :: [Test]
hw2Tests =
        [myFoldl (+) 0 [1..5] ~?= 15
        ,myFoldl (-) 0 [1..5] ~?= -15
        ,myReverse [1..5] ~?= [5,4..1]
        ,myFoldr (+) 0 [1..5] ~?= 15
        ,myFoldr (-) 0 [1..5] ~?= 3
        ,myFoldl2 (+) 0 [1..5] ~?= 15
        ,myFoldl2 (-) 0 [1..5] ~?= -15
        ,and (map isUpper ['A'..'Z']) ~?= True
        ,map isUpper ['1', 'a', 'A'] ~?= [False, False, True]
        ,onlyCapitals1 "Hello, World!" ~?= "HW"
        ,onlyCapitals2 "Hello, World!" ~?= "HW"
        ,onlyCapitals3 "Hello, World!" ~?= "HW"
        ,divRemainder 12 4 ~?= (3,0)
        ,divRemainder 23 5 ~?= (4,3)
        ,digitSum 123 ~?= 6
        ,digitSum (-323) ~?= 8
        ,sayNum "5" ~?= "five "
        ,sayNum "23" ~?= "twenty three "
        ,sayNum "82379" ~?= "eighty two thousand three hundred seventy nine "
        ,sayNum "93218065" ~?= "ninety three million two hundred eighteen thousand sixty five "
        ]
