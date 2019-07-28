module Chp15Ex3 where

import Test.QuickCheck
import Text.Show.Functions

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial
  
instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c )

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x x') (Two y y') = Two (x <> y) (x' <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String (Maybe String) -> Two String (Maybe String) -> Two String (Maybe String) -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj x) (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj x) (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show (Combine f) = show f

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \x -> f x <> g x

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty

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

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- (arbitrary :: (CoArbitrary a, Arbitrary a) => Gen (a -> a))
    return $ Comp f

main :: IO ()
main = do
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String (Maybe String) -> Bool)
  quickCheck (monoidRightIdentity :: Two String (Maybe String) -> Bool)
  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
