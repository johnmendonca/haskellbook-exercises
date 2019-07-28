module Chp20Ex2 where

data Constant a b = Constant a

instance Foldable (Constant a) where
  foldMap f _ = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ x) = f x

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = f x `mappend` f y 

data Four a b = Four a b b b

instance Foldable (Four a) where
  foldMap f (Four _ x y z) = f x `mappend` f y `mappend` f z

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF = undefined
