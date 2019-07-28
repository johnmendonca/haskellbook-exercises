module Chp20Ex1 where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
--sum' = foldr (+) 0
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
--product' = foldr (*) 1
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (==x))
--elem' x = foldr ((||) . (==x)) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr minMaybe Nothing
  where minMaybe x Nothing  = Just x
        minMaybe x (Just y) = Just $ min x y

null' :: (Foldable t) => t a -> Bool
--null' xs = getAll $ foldMap (All . const False) xs
null' = foldr ((&&) . const False) True

length' :: (Foldable t) => t a -> Int
--length' = foldr ((+) . const 1) 0
length' = getSum . foldMap (Sum . const 1)

toList' :: (Foldable t) => t a -> [a]
--toList' = foldr (:) []
toList' = foldMap (: [])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldr' :: Foldable f => (a -> b -> b) -> b -> f a -> b
foldr' f z xs = appEndo (foldMap (Endo . f) xs) z

foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap' f = foldr (mappend . f) mempty

