module FMS.Modules.Library.Routes
  ( libraryRoutes
  , itemsHandler
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, queryString)
import Text.Read (readMaybe)

import FMS.Core.Module (Route, Segment(..), get, jsonResponse, post)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Query (applyItemQuery, queryParams)
import FMS.Modules.Library.Monad (MonadLibrary(..))

-- | The library API (docs/modules.org): list all items, fetch one by id,
-- trigger a rescan.
libraryRoutes :: [Route]
libraryRoutes =
  [ get  [Lit "library"] listItemsHandler
  , get  [Lit "library", Capture "id"] itemHandler
  , post [Lit "library", Lit "rescan"] rescanHandler
  ]

-- | @GET \/library@ — JSON array of all items.
listItemsHandler :: [(Text, Text)] -> Request -> AppM Response
listItemsHandler _captures _req =
  jsonResponse <$> listItems

-- | @GET \/items@ — Jellyfin-shaped items query (docs/todo.org section 4):
-- query params @search@, @sortBy@, @sortOrder@, @startIndex@, @limit@ over
-- the whole library, applied by the pure helpers in "FMS.Core.Query".
-- Responds with the envelope
-- @{\"Items\": [...], \"TotalRecordCount\": n, \"StartIndex\": s}@ where
-- the count is taken /before/ pagination. Invalid integer params are
-- ignored (defaults apply). Unlike @GET \/library@, which stays a plain
-- JSON array, this is the paginated envelope Jellyfin clients expect.
itemsHandler :: [(Text, Text)] -> Request -> AppM Response
itemsHandler _captures req = do
  items <- listItems
  let qparams = queryParams (queryString req)
      (total, page) = applyItemQuery qparams items
      startIndex = fromMaybe (0 :: Int) (lookup "startIndex" qparams >>= readMaybe . T.unpack)
  pure (jsonResponse (object
    [ "Items" .= page
    , "TotalRecordCount" .= total
    , "StartIndex" .= startIndex
    ]))

-- | @GET \/library\/:id@ — one item, or 404.
itemHandler :: [(Text, Text)] -> Request -> AppM Response
itemHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItem i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> pure (jsonResponse item)

-- | @POST \/library\/rescan@ — rescan the library, then @{\"status\":\"ok\"}@.
rescanHandler :: [(Text, Text)] -> Request -> AppM Response
rescanHandler _captures _req = do
  rescanLibrary
  pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
