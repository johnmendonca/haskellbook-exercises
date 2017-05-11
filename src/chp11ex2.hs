module Chp11Ex2 where

startswith :: (Eq a) => [a] -> [a] -> Bool
startswith [] _ = True
startswith _ [] = False
startswith (x:[]) (y:_)  = x == y
startswith (x:xs) (y:ys) = x == y && startswith xs ys

isSubstringOf :: (Eq a) => [a] -> [a] -> Bool
isSubstringOf [] [] = True
isSubstringOf _  [] = False
isSubstringOf sub str@(_:xs) = startswith sub str || isSubstringOf sub xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] []    = True
isSubsequenceOf [] (_:_) = True
isSubsequenceOf _  []    = False
isSubsequenceOf sub@(x:xs) (y:ys)
  | x == y    = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf sub ys

