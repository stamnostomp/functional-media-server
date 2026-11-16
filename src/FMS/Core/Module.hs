module FMS.Core.Module
  ( Segment(..)
  , Route(..)
  , Module(..)
  , get
  , post
  , delete
  , getDoc
  , postDoc
  , deleteDoc
  , jsonResponse
  ) where

import Data.Aeson (ToJSON, encode)
import Data.Text (Text)
import Network.HTTP.Types (Method, status200)
import Network.Wai (Request, Response, responseLBS)

import FMS.Core.Monad (AppM)

-- | One path segment of a route pattern: a literal or a @:capture@.
data Segment = Lit Text | Capture Text
  deriving (Show, Eq)

-- | A single route. Captures bind as @(name, value)@ pairs passed to the
-- handler. 'routeSummary'/'routeDescription' feed the generated OpenAPI
-- spec (milestone 1c, "FMS.Modules.ApiDocs"); the plain 'get'/'post'/
-- 'delete' helpers leave them empty, 'getDoc'/'postDoc'/'deleteDoc' set
-- them.
data Route = Route
  { routeMethod        :: !Method
  , routePattern       :: ![Segment]
  , routeHandler       :: [(Text, Text)] -> Request -> AppM Response
  , routeSummary       :: !Text
  , routeDescription   :: !Text
  }

-- | A self-contained unit of functionality. The core never knows a module
-- exists; modules are plugged in via the list in "FMS.Server".
data Module = Module
  { moduleName   :: !Text
  , moduleInit   :: AppM ()
  , moduleRoutes :: [Route]
  }

get :: [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
get pat h = Route "GET" pat h "" ""

post :: [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
post pat h = Route "POST" pat h "" ""

delete :: [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
delete pat h = Route "DELETE" pat h "" ""

-- | Like 'get' but with a summary + description for the OpenAPI spec
-- (milestone 1c).
getDoc :: Text -> Text -> [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
getDoc s d pat h = Route "GET" pat h s d

-- | Like 'post' but documented.
postDoc :: Text -> Text -> [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
postDoc s d pat h = Route "POST" pat h s d

-- | Like 'delete' but documented.
deleteDoc :: Text -> Text -> [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
deleteDoc s d pat h = Route "DELETE" pat h s d

-- | Convenience for JSON responses.
jsonResponse :: ToJSON a => a -> Response
jsonResponse v = responseLBS status200 [("Content-Type", "application/json")] (encode v)
