module Chp14Ex2 where

import Test.QuickCheck

data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = frequency [(2, return Fulse), (1, return Frue)]


