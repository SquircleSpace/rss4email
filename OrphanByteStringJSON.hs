module OrphanByteStringJSON () where

import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import Data.Aeson

instance ToJSON BSS.ByteString where
  toJSON = toJSON . T.decodeUtf8 . B64.encode
  toEncoding = toEncoding . T.decodeUtf8 . B64.encode

instance FromJSON BSS.ByteString where
  parseJSON v = do
    text <- parseJSON v
    let encodedBS = T.encodeUtf8 text
    case B64.decode encodedBS of
      Left error -> fail error
      Right bs -> return bs

instance ToJSON BSL.ByteString where
  toJSON = toJSON . T.decodeUtf8 . B64.encode . BSL.toStrict
  toEncoding = toEncoding . T.decodeUtf8 . B64.encode . BSL.toStrict

instance FromJSON BSL.ByteString where
  parseJSON v = do
    text <- parseJSON v
    let encodedBS = T.encodeUtf8 text
    case B64.decode encodedBS of
      Left error -> fail error
      Right bs -> return $ BSL.fromStrict bs
