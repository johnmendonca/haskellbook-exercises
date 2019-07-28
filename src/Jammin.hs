module Jammin where

import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)

data JamJars = Jam { fruit :: Fruit, number :: Int } deriving (Eq, Show, Ord)

row1 = Jam Peach 10
row2 = Jam Plum  10
row3 = Jam Apple 10
row4 = Jam Blackberry 10
row5 = Jam Peach 200
row6 = Jam Plum  44
allJam = [row1, row2, row3, row4, row5, row6]

totalJars :: [JamJars] -> Int
totalJars = foldr ((+) . number) 0

mostRow :: [JamJars] -> JamJars
mostRow = maximumBy (\a b -> compare (number a) (number b))

sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy (\a b -> compare (fruit a) (fruit b))

groupJams :: [JamJars] -> [[JamJars]]
groupJams = (groupBy (\a b -> fruit a == fruit b)) . sortJams
