module Hw1 where

import Text.Read

type Book = (String, String, Int)

citeAuthor :: String -> String -> String
citeAuthor f l = l ++ ", " ++ f

initials :: String -> String -> String
initials [] _ = ""
initials _ [] = ""
initials (f:_) (l:_) = f : "." ++ l : "."

title :: Book -> String
title (_, t, _) = t

citeBook :: Book -> String
citeBook (a, t, y) = t ++ " (" ++ a ++ ", " ++ show y ++ ")"

bibliography_rec :: [Book] -> String
bibliography_rec [] = ""
bibliography_rec [b] = citeBook b
bibliography_rec (b : bs) = citeBook b
                            ++ "\n" ++
                            bibliography_rec bs

bibliography_fold :: [Book] -> String
bibliography_fold [] = ""
bibliography_fold (book : books)
        = foldl (\acc b -> acc ++ "\n" ++ citeBook b)
                (citeBook book) books

averageYear :: [Book] -> Int
averageYear books = sum ys `div` length ys
        where ys = map getYear books
              getYear :: Book -> Int
              getYear (_, _, y) = y

isReference :: String -> Bool
isReference ('[':_:"]") = True
isReference _ = False

fromJustOrError :: String -> Maybe a -> a
fromJustOrError _ (Just a) = a
fromJustOrError e _        = error e

invalidRefMsg :: String
invalidRefMsg = "invalid reference format, expected [%d]"

getRefNum :: String -> Int
getRefNum ('[':i:"]") = fromJustOrError invalidRefMsg $ readMaybe [i]
getRefNum _ = error invalidRefMsg

references :: String -> Int
references = length . filter isReference . words

citeText :: [Book] -> String -> String
citeText refs = unwords . map refToCited . words
        where refToCited :: String -> String
              refToCited ref
                    | isReference ref = citeBook $ refs !! (getRefNum ref - 1)
                    | otherwise       = ref
