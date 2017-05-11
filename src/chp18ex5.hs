module Chp18Ex5 where

j :: Monad m => m (m a) -> m a
j mm = mm >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
  a <- ma
  f <- mf
  return $ f a

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (x:xs) f = l2 (:) (f x) (meh xs f) 

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

