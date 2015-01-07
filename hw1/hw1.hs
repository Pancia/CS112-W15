module Hw1 where

citeAuthor :: String -> String -> String
citeAuthor = undefined

initials :: String -> String -> String
initials = undefined

title :: (String, String, Int) -> String
title = undefined

citeBook :: (String, String, Int) -> String
citeBook = undefined

bibliography_rec :: [(String, String, Int)] -> String
bibliography_rec = undefined

bibliography_fold :: [(String, String, Int)] -> String
bibliography_fold = undefined

averageYear :: [(String, String, Int)] -> Int
averageYear = undefined

references :: String -> Int
references = undefined
--txt :: String
--txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
--      "get to their goal, and in the end the thing they want the most ends " ++
--      "up destroying them.  In case of [2] this is a whale..."
--references txt -- -> 3

citeText :: [(String, String, Int)] -> String -> String
citeText = undefined
--let gatsby = ("F. Scott Fitzgerald", "The Great Gatsby", 1925)
--let moby = ("Herman Melville", "Moby Dick", 1851)
--citeText [gatsby, moby] txt
-- "The Great Gatsby (F. Scott Fitzgerald, 1925) and Moby Dick (Herman Melville, 1851) both feature..."
