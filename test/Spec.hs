module Main (main) where

import FMS (name)

main :: IO ()
main = do
  putStrLn ("test suite for " ++ name)
  -- TODO: real tests once the HTTP layer exists (docs/roadmap.org)
