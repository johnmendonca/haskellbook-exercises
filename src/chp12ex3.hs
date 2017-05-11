module Chp12Ex3 where

import Data.Char

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f next 
  where next = f x

weirdAF :: Char -> Maybe (Char, Char)
weirdAF c
  | mod (ord c) 10 == 0 = Nothing
  | otherwise           = Just (c, succ c)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = 
  case result of
    Nothing       -> []
    (Just (y, z)) -> y : myUnfoldr f z
  where result = f x

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (id y, f y)) x

