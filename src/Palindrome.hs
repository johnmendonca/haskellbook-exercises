module Palindrome where

import Control.Monad (forever)
import Data.Char (isLetter, toLower)

isPalindrome :: String -> Bool
isPalindrome str = s == reverse s
  where s = map toLower $ filter isLetter str

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

