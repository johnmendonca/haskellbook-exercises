module Chp12Ex1 where

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing  = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes ((Just a):xs) = a : catMaybes xs

concatMaybes :: Maybe [a] -> Maybe [a] -> Maybe [a]
concatMaybes Nothing _         = Nothing
concatMaybes _ Nothing         = Nothing
concatMaybes (Just a) (Just b) = Just (a ++ b)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe ((Just a):xs) = concatMaybes (Just [a]) (flipMaybe xs)

