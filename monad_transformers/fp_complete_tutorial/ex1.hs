-- Does not compile
#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

type Reader r = ReaderT r Identity

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving Functor

instance (Monad m) => Applicative (ReaderT r m) where 
    pure x = ReaderT $ \r -> pure x
    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ \r -> do
        f <- rmf r
        a <- rma r
        pure $ f a

instance (Monad m) => Monad (ReaderT r m) where 
    return = pure 
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    (ReaderT rma) >>= f = ReaderT $ \r -> do
        a <- rma r
        (runReaderT (f a)) $ r

instance MonadTrans (ReaderT r) where 
    lift :: Monad m => m a -> ReaderT r m a
    lift ma = ReaderT $ \r -> ma

instance MonadIO m => MonadIO (ReaderT r m) where 
    liftIO :: IO a -> ReaderT r m a
    liftIO ioa = ReaderT $ \r -> liftIO ioa

runReader :: Reader r a -> r -> a
runReader r = runIdentity . runReaderT r

ask :: Monad m => ReaderT r m r
ask = ReaderT $ \r -> pure r

main :: IO ()
main = runReaderT main' "Hello World"

main' :: ReaderT String IO ()
main' = do
  lift $ putStrLn "I'm going to tell you a message"
  liftIO $ putStrLn "The message is:"
  message <- ask
  lift $ putStrLn message
