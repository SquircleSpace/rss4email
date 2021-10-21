{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses #-}

module AppMonad
  ( AppMonad(..)
  , runAppMonad
  ) where

import Control.Monad.Except
import Network.HTTP.Req

newtype AppMonad a = AppMonad { unAppMonad :: ExceptT String IO a }

instance Functor AppMonad where
  fmap f v = AppMonad $ fmap f (unAppMonad v)

instance Applicative AppMonad where
  pure v = AppMonad (pure v)
  f <*> v = AppMonad (unAppMonad f <*> unAppMonad v)

instance Monad AppMonad where
  v >>= f = AppMonad (unAppMonad v >>= unAppMonad . f)

instance MonadIO AppMonad where
  liftIO action = AppMonad $ liftIO action

instance MonadHttp AppMonad where
  handleHttpException exception = throwError $ show exception

instance MonadError String AppMonad where
  throwError e = AppMonad $ throwError e
  catchError action f = AppMonad $ catchError (unAppMonad action) (unAppMonad . f)

runAppMonad :: AppMonad a -> IO (Either String a)
runAppMonad action = runExceptT (unAppMonad action)
