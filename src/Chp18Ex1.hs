module Chp18Ex1 where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

