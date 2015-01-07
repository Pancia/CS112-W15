module Hw1 where

type Book = (String, String, Int)

citeAuthor :: String -> String -> String
citeAuthor = undefined

initials :: String -> String -> String
initials = undefined

title :: Book -> String
title = undefined

citeBook :: Book -> String
citeBook = undefined

bibliography_rec :: [Book] -> String
bibliography_rec = undefined

bibliography_fold :: [Book] -> String
bibliography_fold = undefined

averageYear :: [Book] -> Int
averageYear = undefined

references :: String -> Int
references = undefined
--txt :: String
--txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
--      "get to their goal, and in the end the thing they want the most ends " ++
--      "up destroying them.  In case of [2] this is a whale..."
--references txt -- -> 3

citeText :: [Book] -> String -> String
citeText = undefined
--let gatsby = ("F. Scott Fitzgerald", "The Great Gatsby", 1925)
--let moby = ("Herman Melville", "Moby Dick", 1851)
--citeText [gatsby, moby] txt
-- "The Great Gatsby (F. Scott Fitzgerald, 1925) and Moby Dick (Herman Melville, 1851) both feature..."
