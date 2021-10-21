{-# LANGUAGE FlexibleContexts #-}

module Utils
  ( safeHead
  , maybeToEither
  , liftMaybe
  , encodePretty
  ) where

import Control.Monad.Except
import Control.Exception
import Data.ByteString.Lazy as BSL
import qualified Data.Aeson.Encode.Pretty as AEP
import Data.Aeson

safeHead (a:_) = Just a
safeHead [] = Nothing

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither (Just x) _ = Right x
maybeToEither Nothing x = Left x

liftMaybe :: MonadError e m => Maybe a -> e -> m a
liftMaybe maybe e = liftEither $ maybeToEither maybe e

encodeConfig = AEP.defConfig
  { AEP.confCompare = compare
  , AEP.confTrailingNewline = True
  }

encodePretty :: ToJSON a => a -> BSL.ByteString
encodePretty = AEP.encodePretty' encodeConfig
