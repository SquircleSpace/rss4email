{-# LANGUAGE OverloadedStrings,ApplicativeDo,RecordWildCards,TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Email
  ( mailForItem
  , sendEmail
  ) where

import Control.Monad.Except
import Network.Mail.SMTP
import Network.Mail.Mime
import Text.Feed.Types
import Text.Feed.Query
import Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Text.Encoding as TextE
import Data.Maybe
import Data.Aeson.TH
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Char
import Data.String
import qualified Data.ByteString.Lazy as BSL

import FeedConfig
import FeedState
import THUtils
import Utils
import MailJSON

emailDomain :: Text -> Maybe Text
emailDomain t = if Text.null dropped then Nothing else Just dropped
  where dropped = Text.drop 1 $ Text.dropWhile (/= '@') t

mailForItem :: Config -> UTCTime -> Text -> FeedConfig -> Item -> Maybe Mail
mailForItem config now key feedConfig item = let
  mkAddress email = Address { addressEmail = email, addressName = Nothing }
  from = mkAddress $ configFromAddress config
  to = mkAddress $ fromMaybe (configToEmail config) (feedToEmail feedConfig)

  title = fromMaybe "<untitled>" $ getItemTitle item
  description = fromMaybe "No description" $ getItemDescription item
  link = fromMaybe "No link" $ getItemLink item

  subjectPrefix = fromMaybe key $ feedTitle feedConfig
  subject = Text.concat [subjectPrefix, " / ", title]

  nowString = Text.filter isAlphaNum $ fromString $ iso8601Show now

  plainBody = Part
    { partType = "text/plain; charset=utf-8"
    , partEncoding = QuotedPrintableText
    , partDisposition = DefaultDisposition
    , partHeaders = []
    , partContent = PartContent $ BSL.fromStrict $ TextE.encodeUtf8 $ Text.intercalate "\n" [link]
    }
  alternatives = [[plainBody]]
  in do
  hash <- getItemHash item
  domain <- emailDomain $ configFromAddress config
  let messageID = Text.concat [ "rss4email.", nowString, ".", hash, "@", domain]
  let headers =
        [ ("Subject", subject)
        , ("Message-ID", messageID)
        ]
  return Mail
    { mailFrom = from
    , mailTo = [ to ]
    , mailCc = []
    , mailBcc = []
    , mailHeaders = headers
    , mailParts = alternatives
    }

readPassword :: MonadIO m => Text -> m Text
readPassword path = do
  let pathString = unpack path
  password <- liftIO $ TextIO.readFile pathString
  return $ dropWhileEnd isSpace password

getSMTPPassword :: (MonadIO m, MonadError String m) => Config -> m Text
getSMTPPassword config = let
  options = catMaybes
    [ return <$> configSMTPPassword config
    , readPassword <$> configSMTPPasswordFile config
    ]
  choice = safeHead options
  in case choice of
       Nothing -> throwError "No password set in config"
       Just action -> action

sendEmail :: (MonadIO m, MonadError String m) => Config -> Mail -> m ()
sendEmail config mail = do
  password <- getSMTPPassword config
  let port = fromMaybe 25 $ configSMTPServerPort config
  liftIO $ sendMailWithLogin'
    (Text.unpack $ configSMTPServer config)
    (fromInteger port)
    (Text.unpack $ configSMTPUsername config)
    (Text.unpack $ password)
    mail
