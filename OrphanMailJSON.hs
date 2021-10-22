{-# LANGUAGE TemplateHaskell #-}

module OrphanMailJSON () where

import Network.Mail.Mime
import Data.Aeson.TH

import THUtils
import OrphanByteStringJSON

$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Mail)
$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Address)
$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''Part)
$(deriveJSON jsonOptions ''Encoding)
$(deriveJSON jsonOptions ''Disposition)
$(deriveJSON jsonOptions ''PartContent)
