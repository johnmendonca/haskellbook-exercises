module Chp15Ex4 where

import Data.Monoid

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem g) = Mem $ \s -> (fst (f s) <> fst (g s), snd $ f (snd $ g s)) 

f' = Mem $ \s -> ("hi", s + 1)

main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0

