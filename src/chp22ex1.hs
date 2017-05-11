module Chp22Ex1 where

import Control.Applicative

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop = fmap boop doop

bbop = (+) <$> boop <*> doop

duwop = liftA2 (+) boop doop

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a1 a2 = f <$> a1 <*> a2

