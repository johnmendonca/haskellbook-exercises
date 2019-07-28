module Cipher where

import Data.Char

charOffset :: Char -> Int
charOffset c 
  | isUpper c = ord 'A'
  | isLower c = ord 'a'
  | otherwise = 0

normalOrd :: Char -> Int
normalOrd c = ord c - charOffset c

cipherChar :: Int -> Char -> Char
cipherChar shift c
  | isLetter c = ciphered
  | otherwise  = c
  where
    shifted    = normalOrd c + shift
    cycled     = shifted `mod` 26
    denormaled = cycled + charOffset c
    ciphered   = chr denormaled

caesar :: Int -> String -> String
caesar offset str = map (cipherChar offset) str

decaesar :: Int -> String -> String
decaesar offset = caesar (-offset)

type Keyword = String

keywordMask :: Keyword -> String -> [Int]
keywordMask key str = keywordMask' (concat $ repeat key) str

keywordMask' :: String -> String -> [Int]
keywordMask' _ [] = []
keywordMask' key@(k:kx) (x:xs)
  | isLetter x = normalOrd k : keywordMask kx xs
  | otherwise  =           0 : keywordMask key xs

vigenere :: Keyword -> String -> String
vigenere key str = zipWith cipherChar mask str
  where mask = keywordMask key str 

devigenere :: Keyword -> String -> String
devigenere key str = zipWith cipherChar mask str
  where mask = map negate $ keywordMask key str 

