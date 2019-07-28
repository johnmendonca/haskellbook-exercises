module Chp14Ex1 where

import Test.QuickCheck
import Data.List (sort)

half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == halfIdentity x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y, x >= y)

prop_listOrdered :: [String] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

plusAssociative x y z = (x :: Int) + (y + z) == (x + y) + z
plusCommutative x y = (x :: Int) + y == y + x

multAssociative x y z = (x :: Int) * (y * z) == (x * y) * z
multCommutative x y = (x :: Int) * y == y * x

prop_quotRem :: Int -> Positive Int -> Bool
prop_quotRem x (Positive y) = (quot x y) * y + (rem x y) == x

prop_divMod :: Int -> Positive Int -> Bool
prop_divMod x (Positive y) = (div x y) * y + (mod x y) == x

expAssociative :: Int -> Int -> Int -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z
expCommutative :: Int -> Int -> Bool
expCommutative x y = x ^ y == y ^ x

prop_reverse :: [String] -> Bool
prop_reverse xs = (reverse . reverse) xs == xs

prop_foldrCons :: [Int] -> [Int] -> Bool
prop_foldrCons xs ys = foldr (:) xs ys == (++) xs ys

prop_foldrConcat :: [String] -> Bool
prop_foldrConcat xs = foldr (++) [] xs == concat xs

prop_readShow :: (Int, Double) -> Bool
prop_readShow x = (read (show x)) == x

square x = x * (x :: Double)
prop_sqrt x = (square . sqrt) x == x

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck plusAssociative  
  quickCheck plusCommutative
  quickCheck multAssociative  
  quickCheck multCommutative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  --quickCheck expAssociative
  --quickCheck expCommutative
  quickCheck prop_reverse
  --quickCheck prop_foldrCons
  quickCheck prop_foldrConcat
  quickCheck prop_readShow
  --quickCheck prop_sqrt

