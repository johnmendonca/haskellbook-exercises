module Chp12ex2 where

consLeft :: Either a b -> [a] -> [a]
consLeft (Right _) xs = xs
consLeft (Left x)  xs = x : xs

lefts' :: [Either a b] -> [a]
lefts' = foldr consLeft []

consRight :: Either a b -> [b] -> [b]
consRight (Left _)  xs = xs
consRight (Right x) xs = x : xs

rights' :: [Either a b] -> [b]
rights' = foldr consRight []

consEitherTuple :: Either a b -> ([a],[b]) -> ([a],[b])
consEitherTuple (Left x)  (xs, ys) = (x:xs, ys)
consEitherTuple (Right y) (xs, ys) = (xs, y:ys)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr consEitherTuple ([],[])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

