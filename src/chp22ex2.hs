module Chp22Ex2 where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed = cap . rev

fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledM = cap >>= (\a -> rev >>= (\b -> return (a, b)))
--        (r -> a) >>= (\a -> (r -> a) >>= (\b -> return (a, b))) 

tupledD = do
  a <- cap
  b <- rev
  return (a,b)

