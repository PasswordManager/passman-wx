module Passman.EitherT where
import Control.Monad
import Control.Monad.Trans

newtype EitherT e m a = EitherT (m (Either e a))

runEitherT (EitherT x) = x

instance Monad m => Monad (EitherT e m) where
  m >>= k = EitherT $ do
      a <- runEitherT m
      case a of
          Left e -> return (Left e)
          Right x -> runEitherT (k x)
  fail = EitherT . fail
  return = EitherT . return . return

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO
