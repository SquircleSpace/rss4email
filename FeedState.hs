{-# LANGUAGE TemplateHaskell,OverloadedStrings #-}
{-# OPTIONS_GHC -Werror=missing-fields #-}

module FeedState
  ( State(..)
  , emptyState
  , parseStateL
  , parseStateS
  , serializeState
  , FeedState
  , mkFeedStateFromFeed
  , mkFeedStateFromItems
  , emptyFeedState
  , hasSeenItem
  , unseenItems
  , mergeFeedStates
  , diffFeedStates
  , ageOutEntriesFromFeedState
  , getItemHash
  ) where

import Data.ByteString as BSS
import Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Aeson.TH
import Data.Aeson
import Control.Monad
import Data.Maybe
import Data.Map as Map
import Data.Text
import Data.Text.Encoding
import Text.Feed.Types
import Text.Feed.Query
import Data.Time.Clock
import Data.Time.Format
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString.Base64 as B64

import THUtils
import Utils
import MailJSON

data State = State
  { feedStates :: Map Text FeedState
  , outbox :: [MailJSON]
  } deriving (Show)

newtype Date = Date { unDate :: UTCTime }
  deriving (Show, Eq, Ord)

mkDate day time = Date $ UTCTime day time

instance ToJSON Date where
  toJSON (Date utc) = object ["day" .= toJSON (utctDay utc), "time" .= toJSON (utctDayTime utc)]
  toEncoding (Date utc) = pairs ("day" .= toJSON (utctDay utc) <> "time" .= toJSON (utctDayTime utc))

instance FromJSON Date where
  parseJSON = withObject "Date" $ \d -> mkDate <$> d .: "day" <*> d .: "time"

type SeenMap = Map Text Date

data FeedState = FeedState
  { feedSeenGuids :: SeenMap
  , feedSeenLinks :: SeenMap
  , feedSeenHashes :: SeenMap
  } deriving (Show)

$(deriveJSON jsonOptions { fieldLabelModifier = dropCamelLower 1 } ''FeedState)
$(deriveJSON jsonOptions ''State)

emptyState = State
  { feedStates = Map.empty
  , outbox = []
  }

parseStateL :: BSL.ByteString -> Either String State
parseStateL = eitherDecode'

parseStateS :: BSS.ByteString -> Either String State
parseStateS = eitherDecode' . BSL.fromStrict

serializeState :: State -> BSL.ByteString
serializeState = encodePretty

emptyFeedState = FeedState
  { feedSeenGuids = Map.empty
  , feedSeenLinks = Map.empty
  , feedSeenHashes = Map.empty
  }

data MeldFunctions = MeldFunctions
  { meldSeenGuids :: Map Text Date -> Map Text Date -> Map Text Date
  , meldSeenLinks :: Map Text Date -> Map Text Date -> Map Text Date
  , meldSeenHashes :: Map Text Date -> Map Text Date -> Map Text Date
  }

liftMeld :: (a -> a -> a) -> (FeedState -> a) -> FeedState -> FeedState -> a
liftMeld meld getter left right = meld (getter left) (getter right)

updateWithMeld meld left right = FeedState
  { feedSeenGuids = liftMeld (meldSeenGuids meld) feedSeenGuids left right
  , feedSeenLinks = liftMeld (meldSeenLinks meld) feedSeenLinks left right
  , feedSeenHashes = liftMeld (meldSeenHashes meld) feedSeenHashes left right
  }

newtype UnionMeld = UnionMeld { unUnionMeld :: FeedState }
  deriving (Show)

laterDate left right = if left < right then right else left

unionMeld = MeldFunctions
  { meldSeenGuids = unionWith laterDate
  , meldSeenLinks = unionWith laterDate
  , meldSeenHashes = unionWith laterDate
  }

instance Semigroup UnionMeld where
  (UnionMeld left) <> (UnionMeld right) = UnionMeld $ updateWithMeld unionMeld left right

instance Monoid UnionMeld where
  mempty = UnionMeld emptyFeedState

newtype DifferenceMeld = DifferenceMeld { unDifferenceMeld :: FeedState }

differenceMeld = MeldFunctions
  { meldSeenGuids = difference
  , meldSeenLinks = difference
  , meldSeenHashes = difference
  }

instance Semigroup DifferenceMeld where
  (DifferenceMeld left) <> (DifferenceMeld right) = DifferenceMeld $ updateWithMeld differenceMeld left right

instance Monoid DifferenceMeld where
  mempty = DifferenceMeld emptyFeedState

itemKeys :: [(Item -> Maybe Text, FeedState -> SeenMap, SeenMap -> FeedState -> FeedState)]
itemKeys =
  [ (getItemGuid, feedSeenGuids, setFeedSeenGuids)
  , (getItemLink, feedSeenLinks, setFeedSeenLinks)
  , (getItemHash, feedSeenHashes, setFeedSeenHashes)
  ]

hasSeenItem :: FeedState -> Item -> Bool
hasSeenItem fs item = fromMaybe False $ safeHead $ do
  (itemGetter, mapGetter, _) <- itemKeys
  key <- maybeToList $ itemGetter item
  return $ member key (mapGetter fs)

unseenItems :: FeedState -> Feed -> [Item]
unseenItems fs feed = Prelude.filter (not . hasSeenItem fs) $ feedItems feed

mkStateFromItem :: UTCTime -> Item -> Maybe FeedState
mkStateFromItem now item = safeHead $ do
  (itemGetter, _, mapSetter) <- itemKeys
  key <- maybeToList $ itemGetter item
  let newMap = Map.fromList [(key, Date now)]
  return $ mapSetter newMap emptyFeedState

mkFeedStateFromItems :: UTCTime -> [Item] -> FeedState
mkFeedStateFromItems now items = unUnionMeld $ mconcat states
  where
    states = Data.Maybe.mapMaybe (\i -> UnionMeld <$> mkStateFromItem now i) items

mkFeedStateFromFeed :: UTCTime -> Feed -> FeedState
mkFeedStateFromFeed now feed = mkFeedStateFromItems now $ feedItems feed

mergeFeedStates :: FeedState -> FeedState -> FeedState
mergeFeedStates left right = unUnionMeld $ (UnionMeld left) <> (UnionMeld right)

diffFeedStates :: FeedState -> FeedState -> FeedState
diffFeedStates left right = unDifferenceMeld $ (DifferenceMeld left) <> (DifferenceMeld right)

-- Okay, its not really for melding.  Shoot me.
ageOutEntriesMeldFunctions oldestTime = MeldFunctions
  { meldSeenGuids = fn
  , meldSeenLinks = fn
  , meldSeenHashes = fn
  }
  where fn map _ = Map.filter (\age -> unDate age > oldestTime) map

ageOutEntriesFromFeedState :: UTCTime -> FeedState -> FeedState
ageOutEntriesFromFeedState oldestTime feedState =
  updateWithMeld (ageOutEntriesMeldFunctions oldestTime) feedState feedState

type FeedStateUpdater a = a -> FeedState -> FeedState

setFeedSeenGuids map fs = fs { feedSeenGuids = map }
setFeedSeenLinks map fs = fs { feedSeenLinks = map }
setFeedSeenHashes map fs = fs { feedSeenHashes = map }

getItemGuid item = snd <$> getItemId item

hashGetters =
  [ getItemLink
  , getItemTitle
  , getItemDescription
  ]

getItemHash item =
  case strings of
    [] -> Nothing
    l -> Just $ decodeUtf8 $ B64.encode $ SHA.finalize $ SHA.updates SHA.init byteStrings
  where
    byteStrings = Prelude.map encodeUtf8 strings
    strings = do
      getter <- hashGetters
      maybeToList $ getter item
