{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Chp11Ex1 where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (x, _) = tooMany x

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany x && tooMany y

