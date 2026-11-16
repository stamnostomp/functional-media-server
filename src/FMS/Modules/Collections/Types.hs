module FMS.Modules.Collections.Types
  ( Collection(..)
  , boxSetIdPrefix
  , boxSetIdFor
  , isBoxSetId
  , boxSetItemValue
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | One collection / box set (milestone 7): an ordered list of library
-- item ids with a name — the Playlists shape, persisted in
-- @stateDir/collections.json@. Collections also surface as BoxSet
-- pseudo-items in @/items@ (see 'boxSetItemValue').
data Collection = Collection
  { colId       :: !Text       -- ^ minted server-side (random hex)
  , colName     :: !Text
  , colItemIds  :: ![Text]     -- ^ library item ids, in collection order
  , colAddedAt  :: !UTCTime
  } deriving (Eq, Show)

-- | API JSON with Jellyfin-style PascalCase keys (same as 'Playlist').
instance ToJSON Collection where
  toJSON c = object
    [ "Id"       .= colId c
    , "Name"     .= colName c
    , "ItemIds"  .= colItemIds c
    , "AddedAt"  .= colAddedAt c
    ]

instance FromJSON Collection where
  parseJSON = withObject "Collection" $ \o ->
    Collection
      <$> o .:  "Id"
      <*> o .:  "Name"
      <*> o .:? "ItemIds" .!= []
      <*> o .:  "AddedAt"

-- | Prefix of the synthetic BoxSet item ids in @/items@.
boxSetIdPrefix :: Text
boxSetIdPrefix = "boxset-"

-- | The pseudo-item id of a collection: @\"boxset-\" <> colId@.
boxSetIdFor :: Text -> Text
boxSetIdFor cid = boxSetIdPrefix <> cid

-- | Does this item id refer to a BoxSet pseudo-item (not a library item)?
isBoxSetId :: Text -> Bool
isBoxSetId = T.isPrefixOf boxSetIdPrefix

-- | The pseudo-item JSON of a collection for @/items@ and
-- @GET /items/:id@: MediaItem-style lowercase keys (@id@, @title@,
-- @type@) with @id = \"boxset-\" <> colId@ and @type = \"BoxSet\"@, plus
-- @itemCount@.
boxSetItemValue :: Collection -> Value
boxSetItemValue c = object
  [ "id"        .= boxSetIdFor (colId c)
  , "title"     .= colName c
  , "type"      .= ("BoxSet" :: Text)
  , "itemCount" .= length (colItemIds c)
  ]
