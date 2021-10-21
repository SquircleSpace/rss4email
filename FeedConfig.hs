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
  { configFeedConfigs :: Map Text FeedConfig
  , configToEmail :: Text
  , configSMTPServer :: Text
  , configSMTPUsername :: Text
  , configSMTPPasswordCommand :: Maybe [Text]
  , configSMTPPasswordFile :: Maybe Text
  , configSMTPPassword :: Maybe Text
  } deriving (Show)

exampleConfig = Config
  { configFeedConfigs = Map.empty
  , configToEmail = "cool-person@example.com"
  , configSMTPServer = "smtp.example.com"
  , configSMTPUsername = "username"
  , configSMTPPasswordCommand = Nothing
  , configSMTPPasswordFile = Just "/var/lib/rss4email/smtpPassword"
  , configSMTPPassword = Nothing
  }

data FeedConfig = FeedConfig
  { feedTitle :: Maybe Text
  , feedURL :: Text
  , feedToEmail :: Maybe Text
  } deriving (Show)

$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Config)
$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''FeedConfig)

parseConfigL :: BSL.ByteString -> Either String Config
parseConfigL = eitherDecode'

parseConfigS :: BSS.ByteString -> Either String Config
parseConfigS = eitherDecode' . BSL.fromStrict

serializeConfig :: Config -> BSL.ByteString
serializeConfig = encodePretty
