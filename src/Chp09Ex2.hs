module Chp09Ex2 where

import Data.Char

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

