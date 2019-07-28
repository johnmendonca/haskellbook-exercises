module Chp21Ex2 where

import Control.Applicative

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x `mappend` foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = Three x y <$> f z

data Three' a b = Three' a b b deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y `mappend` f z

instance Traversable (Three' a) where
  traverse f (Three' x y z) = Three' x <$> f y <*> f z

data S n a = S (n a) a deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
  fmap f (S m x) = S (f <$> m) (f x)

--instance Foldable n => Foldable (S n) where
--  foldMap f (S m x) = mappend (foldMap f m) x
