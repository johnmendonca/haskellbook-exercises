module Chp15Ex1 where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada x = x
  (<>) x Nada = x
  (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Only a]
  
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) x = x
  (<>) x (First' Nada) = x
  (<>) (First' (Only x)) (First' (Only y)) = First' (Only x)

instance Monoid (First' a) where
  mempty = First' Nada

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return (First' a)

firstMappend :: First' a -> First' a -> First' a
firstMappend = (<>)

type FirstMappend =
     First' (Optional String)
  -> First' (Optional String)
  -> First' (Optional String)
  -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' (Optional String) -> Bool)
  quickCheck (monoidRightIdentity :: First' (Optional String) -> Bool)
