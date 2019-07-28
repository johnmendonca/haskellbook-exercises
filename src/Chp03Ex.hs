module Chp03Ex where

addBang str = str ++ "!"

getFifth str = str !! 4

dropNine str = drop 9 str

thirdLetter str = str !! 2

letterIndex x = "Curry is awesome!" !! x

rvrs str = awesome ++ is ++ curry
  where
    curry = take 5 str
    is = take 4 (drop 5 str)
    awesome = drop 9 str

main :: IO ()
main = print $ rvrs "Curry is awesome!"

