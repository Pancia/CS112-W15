module Hw1 where

import Text.Read

type Book = (String, String, Int)

citeAuthor :: String -> String -> String
citeAuthor first last = last ++ ", " ++ first

initials :: String -> String -> String
initials first last = [head first] ++ "." ++ [head last] ++ "."

title :: Book -> String
title (_, t, _) = t

citeBook :: Book -> String
citeBook (a, t, y) = t ++ " (" ++ a ++ ", " ++ show y ++ ")"

bibliography_rec :: [Book] -> String
bibliography_rec (b : []) = citeBook b
bibliography_rec (b : bs) = citeBook b ++ "\n" ++ bibliography_rec bs

bibliography_fold :: [Book] -> String
bibliography_fold (book : books)
        = foldl (\acc b -> acc ++ "\n" ++ citeBook b)
                (citeBook book) books

averageYear :: [Book] -> Int
averageYear books = sum ys `div` length ys
        where ys = map getYear books
              getYear (_, _, y) = y

isReference :: String -> Bool
isReference ('[':_:"]") = True
isReference word = False

getRefNum :: String -> Int
getRefNum ('[':i:"]") = fromJust $ readMaybe [i]
        where fromJust :: Maybe a -> a
              fromJust (Just a) = a
              fromJust a        = error "invalid reference format"
getRefNum ref = error "invalid reference format"

references :: String -> Int
references = length . filter isReference . words

citeText :: [Book] -> String -> String
citeText refs = unwords . map refToCited . words
        where refToCited :: String -> String
              refToCited ref
                    | isReference ref = citeBook $ refs !! (getRefNum ref - 1)
                    | otherwise       = ref
