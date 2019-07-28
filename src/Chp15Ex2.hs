{-# LANGUAGE DeriveGeneric #-}

module Chp15Ex2 where

import GHC.Generics
import Data.Semigroup
import Test.QuickCheck
import Text.Show.Functions

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c )

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x x') (Two y y') = Two (x <> y) (x' <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String (Maybe String) -> Two String (Maybe String) -> Two String (Maybe String) -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj x) (BoolConj y) = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj x) (BoolDisj y) = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Fst _) x@(Fst _) = x
  (<>) (Fst _) x@(Snd _) = x
  (<>) x@(Snd _) _       = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc = Or BoolConj BoolDisj -> Or BoolConj BoolDisj -> Or BoolConj BoolDisj ->  Bool

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show (Combine f) = show f

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- (arbitrary :: (CoArbitrary a, Arbitrary b) => Gen (a -> b))
    return $ Combine f

combineAssoc :: Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool
combineAssoc a b c x = unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c ) x

newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show (Comp f) = show f

instance Semigroup (Comp a) where
  (<>) (Comp f) (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- (arbitrary :: (CoArbitrary a, Arbitrary a) => Gen (a -> a))
    return $ Comp f

compAssoc :: Comp String -> Comp String -> Comp String -> String -> Bool
compAssoc a b c x = unComp (a <> (b <> c)) x == unComp ((a <> b) <> c ) x

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) x@(Success' _) (Success' _) = x
  (<>) x@(Failure' _) (Success' _) = x
  (<>) (Success' _) x@(Failure' _) = x
  (<>) (Failure' x) (Failure' y)   = Failure' (x <> y)

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (<>) (AccumulateRight (Success' x))   (AccumulateRight (Success' y))   = AccumulateRight (Success' (x <> y))
  (<>) (AccumulateRight (Failure' _))   x@(AccumulateRight (Failure' _)) = x
  (<>) x@(AccumulateRight (Success' _)) _                                = x
  (<>) _                                x@(AccumulateRight (Success' _)) = x

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success' x))   (AccumulateBoth (Success' y))   = AccumulateBoth (Success' (x <> y))
  (<>) (AccumulateBoth (Failure' x))   (AccumulateBoth (Failure' y))   = AccumulateBoth (Failure' (x <> y))
  (<>) x@(AccumulateBoth (Success' _)) _                               = x
  (<>) _                               x@(AccumulateBoth (Success' _)) = x

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck combineAssoc
  quickCheck compAssoc

