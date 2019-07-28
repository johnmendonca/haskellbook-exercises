module Chp26Ex5 where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

rShow :: Show a => Reader a String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStr "Hi: "
  print r
  return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStr "Hi: "
  print s
  return (show s, s+1)
  
