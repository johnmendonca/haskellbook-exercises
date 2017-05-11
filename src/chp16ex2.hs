module Chp16Ex2 where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                      (a -> b)
                   -> (b -> c)
                   -> f a
                   -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                      f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

type ThreeFC' = Three' Int Int -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])
  quickCheck (functorCompose' :: IntFC)
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck (functorCompose' :: IdentityFC)
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck (functorCompose' :: PairFC)
  quickCheck $ \x -> functorIdentity (x :: Two Int Int)
  quickCheck (functorCompose' :: TwoFC)
  quickCheck $ \x -> functorIdentity (x :: Three Int Int Int)
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck $ \x -> functorIdentity (x :: Three' Int Int)
  quickCheck (functorCompose' :: ThreeFC')

