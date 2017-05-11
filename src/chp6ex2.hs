module Chp6Ex2 where

import Data.List (sort)

data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "f" "r"
s2 = Sentence "j" "luv" "doge"

i :: Num a => a
i = 1

f :: RealFrac a => a 
f = 1.0

freud :: Ord a => a -> a
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

jung :: [Char] -> Char
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: Ord a => [a] -> a
signifier xs = head xs

foo = signifier (mySort ['s', 'e', 'e'])

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = (f y) + (fromInteger x)

