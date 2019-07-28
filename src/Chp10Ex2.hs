module Chp10Ex2 where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

--theDatabase :: [DatabaseItem]
--theDatabase = _
