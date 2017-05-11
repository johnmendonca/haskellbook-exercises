{-# LANGUAGE OverloadedStrings #-}

module Chp26Ex7 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = case (M.lookup k m) of
  Nothing -> (M.insert k 1 m, 1)
  Just x  -> (M.insert k (x+1) m, x+1)

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key" :: Handler Text
--  let key' = mappend undefined unprefixed
--  newInteger <- undefined
  html $ mconcat [ "<h1>Success! Count was: "
                 , TL.pack $ show 5 --newInteger
                 , "</h1>" ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter "pre_"
      runR rdr = do
        x <- runReaderT rdr config
        modifyIORef' counter id
        return x
  scottyT 3000 runR app

