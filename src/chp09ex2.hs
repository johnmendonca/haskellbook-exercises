module Chp9Ex2 where

import Data.Char

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

