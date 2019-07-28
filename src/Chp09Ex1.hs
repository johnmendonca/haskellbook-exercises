module Chp09Ex1 where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

myWords :: String -> [String]
myWords ""  = [] 
myWords str = word : myWords rest
  where word = takeWhile (/= ' ') str
        rest = dropWhile (== ' ') (dropWhile (/= ' ') str)

