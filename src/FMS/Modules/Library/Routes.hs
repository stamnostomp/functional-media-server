module FMS.Modules.Library.Routes
  ( libraryRoutes
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.Wai (Request, Response)

import FMS.Core.Module (Route, Segment(..), get, jsonResponse, post)
import FMS.Core.Monad (AppError(..), AppM)
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
