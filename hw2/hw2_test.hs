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
        ,sayNum "0" ~?= "zero "
        ,sayNum "5" ~?= "five "
        ,sayNum "23" ~?= "twenty three "
        ,sayNum "230" ~?= "two hundred thirty "
        ,sayNum "13016" ~?= "thirteen thousand sixteen "
        ,sayNum "82379" ~?= "eighty two thousand three hundred seventy nine "
        ,sayNum "93218065" ~?= "ninety three million two hundred eighteen thousand sixty five "
        ,sayNum "298732619826139659876129872356978162498713957197613698162491871352" ~?=
        "two hundred ninety eight vigintillion seven hundred thirty two novemdecillion " ++
        "six hundred nineteen octodecillion eight hundred twenty six septendecillion " ++
        "one hundred thirty nine sexdecillion six hundred fifty nine quindecillion " ++
        "eight hundred seventy six quattuordecillion one hundred twenty nine tredecillion " ++
        "eight hundred seventy two duodecillion three hundred fifty six undecillion " ++
        "nine hundred seventy eight decillion one hundred sixty two nonillion " ++
        "four hundred ninety eight octillion seven hundred thirteen septillion " ++
        "nine hundred fifty seven sextillion one hundred ninety seven quintillion " ++
        "six hundred thirteen quadrillion six hundred ninety eight trillion " ++
        "one hundred sixty two billion four hundred ninety one million eight hundred " ++
        "seventy one thousand three hundred fifty two "
        ]
