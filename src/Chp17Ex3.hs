module Chp17Ex3 where

import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First x) _           = First x
  (<*>) _ (First x)           = First x
  (<*>) (Second f) (Second x) = Second (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (First a)), (1, return (Second b))]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

data Validation e a =
    Error e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Error x)   = Error x
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Error x) (Error y)     = Error (x <> y)
  (<*>) (Error x) _             = Error x
  (<*>) _ (Error x)             = Error x
  (<*>) (Success f) (Success x) = Success (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Error a)), (1, return (Success b))]

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative ((First ("b", "w", (1 :: Int))) :: Sum ([Char], [Char], Int) ([Char], [Char], Int))
  quickBatch $ applicative ((Second ("b", "w", (1 :: Int))) :: Sum ([Char], [Char], Int) ([Char], [Char], Int))
  quickBatch $ applicative ((Error ("b", "w", "z")) :: Validation ([Char], [Char], [Char]) ([Char], [Char], [Char]))
  quickBatch $ applicative ((Success ("b", "w", "z")) :: Validation ([Char], [Char], [Char]) ([Char], [Char], [Char]))
