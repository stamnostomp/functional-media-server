module FMS.Server
  ( runServer
  ) where

import FMS (name, version)

-- | Entry point for the media server. Currently a placeholder; the HTTP
-- layer lands in milestone 1 (see docs/roadmap.org).
runServer :: IO ()
runServer = putStrLn (name ++ " " ++ version ++ ": server not implemented yet")
