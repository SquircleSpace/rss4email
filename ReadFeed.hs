{-# LANGUAGE DataKinds,FlexibleContexts,FlexibleInstances #-}

module ReadFeed
  ( fetchFromURL
  ) where

import Network.HTTP.Req
import Data.ByteString.Lazy
import Control.Monad.Except
import Text.URI
import Text.Feed.Import
import Text.Feed.Types
import Data.Maybe

import Utils

fetchFromURL_ :: (MonadIO m, MonadHttp m) => (Url scheme, Option scheme) -> m ByteString
fetchFromURL_ (url, options) = do
  response <- req GET url NoReqBody lbsResponse options
  return $ responseBody response

fetchFromURL :: (MonadIO m, MonadError String m, MonadHttp m) => URI -> m Feed
fetchFromURL uri = do
  let maybeURL = useURI uri
  urlEither <- liftMaybe maybeURL "Oh no that URI isn't gonna work"
  byteString <- either fetchFromURL_ fetchFromURL_ urlEither
  liftMaybe (parseFeedSource byteString) "What you doing?  That isn't a feed!"
