module Chp26Ex1 where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a =
  EitherT { runEither :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ pure (Right x)

  (EitherT mf) <*> (EitherT ma) = EitherT $ (<*>) <$> mf <*> ma

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      (Left e) -> return (Left e)
      (Right a) -> runEither (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mab) = do
  v <- mab
  case v of
    (Left a) -> f a
    (Right b) -> g b

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

