module FMS.Core.Module
  ( Segment(..)
  , Route(..)
  , Module(..)
  , get
  , post
  , delete
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
-- handler.
data Route = Route
  { routeMethod  :: !Method
  , routePattern :: ![Segment]
  , routeHandler :: [(Text, Text)] -> Request -> AppM Response
  }

-- | A self-contained unit of functionality. The core never knows a module
-- exists; modules are plugged in via the list in "FMS.Server".
data Module = Module
  { moduleName   :: !Text
  , moduleInit   :: AppM ()
  , moduleRoutes :: [Route]
  }

get :: [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
get pat h = Route "GET" pat h

post :: [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
post pat h = Route "POST" pat h

delete :: [Segment] -> ([(Text, Text)] -> Request -> AppM Response) -> Route
delete pat h = Route "DELETE" pat h

-- | Convenience for JSON responses.
jsonResponse :: ToJSON a => a -> Response
jsonResponse v = responseLBS status200 [("Content-Type", "application/json")] (encode v)
