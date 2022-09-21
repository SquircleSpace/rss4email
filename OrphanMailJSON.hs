{-# LANGUAGE TemplateHaskell #-}

module OrphanMailJSON () where

import Network.Mail.Mime
import Data.Aeson
import Data.Aeson.TH

import THUtils
import OrphanByteStringJSON

$(deriveJSON jsonOptions ''Network.Mail.Mime.Encoding)
$(deriveJSON jsonOptions ''Disposition)
$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Address)
$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Mail)

instance ToJSON PartContent where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON PartContent where
  parseJSON = genericParseJSON jsonOptions

partOptions = jsonOptions { fieldLabelModifier = dropCamelLower 1 }

instance ToJSON Part where
  toEncoding = genericToEncoding partOptions

instance FromJSON Part where
  parseJSON = genericParseJSON partOptions

-- $(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Part)
-- $(deriveJSON jsonOptions ''PartContent)

