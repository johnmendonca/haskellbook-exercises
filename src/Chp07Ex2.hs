module Chp07Ex2 where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xlast = fst . divMod x $ 10
        d     = snd . divMod xlast $ 10

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print ((roundTrip 4) :: Int)
  print (id 4)

