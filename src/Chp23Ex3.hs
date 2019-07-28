{-# LANGUAGE InstanceSigs #-}

module Chp23Ex3 where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (x, y) = g s
    in (f x, y)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (fab, s') = f s
        (a, s'') = g s'
    in (fab a, s'') 

instance Monad (Moi s) where
  return = pure
  
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (x, s') = f s
        sas = runMoi $ g x
    in sas s'

altAp f a = do
  f' <- f
  a' <- a
  return (f' a')

altAp' f a = f >>= (\f' -> a >>= (\a' -> return (f' a') ))

stFn :: (Show a, Fractional a) => a -> (String, a)
stFn n = (show n, n - 0.1)

weirdFn :: (Show a, Read a, Fractional a) => String -> Moi a String
weirdFn str = Moi $ \s -> (show s, s + nnum)
  where nnum :: (Read a) => a
        nnum = read str

stFF :: (Show a, Read a, Fractional a) => a -> (String -> a, a)
stFF n = ((+0.05) . read, n - 0.01)
