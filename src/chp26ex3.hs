module Chp26Ex3 where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> someFunc <$> sma s
    where
      someFunc (a, s') = (f a, s')

instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (StateT smf) <*> (StateT sma) = StateT $ \s -> do
    (f, s') <- smf s
    (a, s'') <- sma s'
    return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
