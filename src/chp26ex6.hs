module Chp26Ex6 where

import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class

isValid :: String -> Bool
isValid = elem '!'

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "Say something exciting: "
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "Moar excite pls"
    Just e -> putStrLn ("Sufficient excite: " ++ e)

