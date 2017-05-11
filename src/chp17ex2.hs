module Chp17Ex2 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Show, Eq)

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

append' :: List a -> List a -> List a
append' Nil xs = xs
append' (Cons x xs) ys = Cons x $ xs `append'` ys

fold' :: (a -> b -> b) -> b -> List a -> b
fold' _ b Nil = b
fold' f b (Cons h t) = f h (fold' f b t)

concat' :: List (List a) -> List a
concat' = fold' append' Nil

flatMap' :: (a -> List b) -> List a -> List b
flatMap' f as = concat' (fmap f as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nil), (3, return (Cons a Nil))]

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) xs ys = flatMap' (\f -> fmap f ys) xs

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 30 l
          ys' = let (ZipList' l) = ys
                in take' 30 l
 
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  (<*>) (ZipList' xs) (ZipList' ys) = ZipList' $ zipWith' id xs ys

main :: IO ()
main = do
  --quickBatch (monoid Twoo)
  quickBatch $ applicative (ZipList' (Cons ("b", "w", (1 :: Int)) Nil))
