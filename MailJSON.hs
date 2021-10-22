{-# LANGUAGE TemplateHaskell #-}

module MailJSON (MailJSON(..)) where

import Network.Mail.Mime
import Data.Aeson.TH

import THUtils
import OrphanMailJSON

newtype MailJSON = MailJSON { unMailJSON :: Mail }
  deriving (Show)

$(deriveJSON jsonOptions { unwrapUnaryRecords = True } ''MailJSON)
