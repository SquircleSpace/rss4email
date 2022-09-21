{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

module FeedConfig
  ( Config(..)
  , exampleConfig
  , parseConfigS
  , parseConfigL
  , serializeConfig
  , FeedConfig(..)
  ) where

import Data.ByteString as BSS
import Data.ByteString.Lazy as BSL
import Data.Map as Map
import Data.Aeson.TH
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AEP
import Data.Text
import THUtils
import Utils

data Config = Config
  { configFeeds :: Map Text FeedConfig
  , configFromAddress :: Text
  , configSMTPPasswordFile :: Maybe Text
  , configSMTPPassword :: Maybe Text
  , configSMTPServerPort :: Maybe Integer
  , configSMTPServer :: Text
  , configSMTPUsername :: Text
  , configToEmail :: Text
  } deriving (Show)

exampleConfig = Config
  { configFeeds = Map.empty
  , configFromAddress = "rss4email@example.com"
  , configSMTPPasswordFile = Just "/var/lib/rss4email/smtpPassword"
  , configSMTPPassword = Nothing
  , configSMTPServerPort = Just 25
  , configSMTPServer = "smtp.example.com"
  , configSMTPUsername = "username"
  , configToEmail = "cool-person@example.com"
  }

data FeedConfig = FeedConfig
  { feedTitle :: Maybe Text
  , feedURL :: Text
  , feedToEmail :: Maybe Text
  } deriving (Show)

$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''FeedConfig)
$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Config)

parseConfigL :: BSL.ByteString -> Either String Config
parseConfigL = eitherDecode'

parseConfigS :: BSS.ByteString -> Either String Config
parseConfigS = eitherDecode' . BSL.fromStrict

serializeConfig :: Config -> BSL.ByteString
serializeConfig = encodePretty
