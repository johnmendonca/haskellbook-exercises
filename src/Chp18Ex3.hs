module Chp18Ex3 where

import Data.Monoid ((<>))
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

data SumB b a =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (SumB b) where
  fmap f (First x)  = First (f x)
  fmap _ (Second x) = Second x

instance Applicative (SumB b) where
  pure = First
  (<*>) (Second x) _           = Second x
  (<*>) _ (Second x)           = Second x
  (<*>) (First f) (First x) = First (f x)

instance Monad (SumB a) where
  return = pure
  (>>=) (Second x) _ = Second x
  (>>=) (First x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (SumB b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Second b)), (1, return (First a))]

instance (Eq a, Eq b) => EqProp (SumB a b) where
  (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x  

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monad (NopeDotJpg :: Nope (Int, Int, Int))
  quickBatch $ monad (Second ("b", "w", 1 :: Int) :: SumB (String, String, Int) (String, String, Int))
  quickBatch $ monad (First ("b", "w", 1 :: Int) :: SumB (String, String, Int) (String, String, Int))
  --quickBatch $ monad (Identity ("b", "w", "z"))
