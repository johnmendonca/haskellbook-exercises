module Chp25Ex2 where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (c -> d) -> p a c -> p a d
  second = bimap id

data Deux a b = Deux a b deriving (Show, Eq)

instance Bifunctor Deux where
  bimap f g (Deux a c) = Deux (f a) (g c)

data Const a b = Const a deriving (Show, Eq)

instance Bifunctor Const where
  bimap f _ (Const x) = Const (f x)

data Drei a b c = Drei a b c deriving (Show, Eq)

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b deriving (Show, Eq)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a deriving (Show, Eq)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x 

data Quadriceps a b c d = Quadzzz a b c d deriving (Show, Eq)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz w x y z) = Quadzzz w x (f y) (g z)

instance Bifunctor Either where
  bimap f _ (Left x) = Left (f x)
  bimap _ g (Right x) = Right (g x)
