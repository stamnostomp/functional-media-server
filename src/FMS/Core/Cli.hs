module FMS.Core.Cli
  ( parseCliArgs
  , cliUsage
  ) where

import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T

-- | Pure CLI argument parser (docs/todo.org section 1, milestone 1d).
-- Maps flags to @(key, value)@ pairs that app/Main.hs layers on top of
-- the env vars and config.json (precedence: CLI > env > config.json >
-- defaults). Both @--flag value@ and @--flag=value@ forms are accepted;
-- boolean flags (@--require-auth@, @--watch@, @--follow-symlinks@) take
-- an optional @=true|false@ (a bare flag means true).
--
-- An unknown option or a missing value is a 'Left' with an ASCII message.
parseCliArgs :: [String] -> Either Text [(String, String)]
parseCliArgs = go []
  where
    go acc [] = Right (reverse acc)
    go acc (a : rest)
      | a == "--help"    = Right (reverse (("help", "") : acc))
      | a == "--version" = Right (reverse (("version", "") : acc))
      | a == "--no-security-headers" = go (("security-headers", "false") : acc) rest
      | Just (flag, mval) <- splitFlag a =
          case lookup flag flagSpecs of
            Nothing ->
              Left ("unknown option: --" <> T.pack flag <> " (see --help)")
            Just (key, wantsValue) ->
              case mval of
                Just v -> go ((key, v) : acc) rest
                Nothing
                  | wantsValue ->
                      case rest of
                        (v : rest')
                          | not ("--" `isPrefixOf` v) -> go ((key, v) : acc) rest'
                        _ -> Left ("option --" <> T.pack flag <> " requires a value")
                  | otherwise -> go ((key, "true") : acc) rest  -- bare boolean flag
      | otherwise = Left ("unknown option: " <> T.pack a <> " (see --help)")

    splitFlag a = case break (== '=') a of
      (f, '=' : v) | Just f' <- stripDashes f -> Just (f', Just v)
      (f, [])      | Just f' <- stripDashes f -> Just (f', Nothing)
      _ -> Nothing

    stripDashes ('-' : '-' : f) = Just f
    stripDashes _ = Nothing

    -- flag name -> (canonical key, takes a mandatory value?)
    flagSpecs =
      [ ("port",                ("port", True))
      , ("library-path",        ("library-path", True))
      , ("state-dir",           ("state-dir", True))
      , ("require-auth",        ("require-auth", False))
      , ("server-name",         ("server-name", True))
      , ("tmdb-api-key",        ("tmdb-api-key", True))
      , ("rate-limit",          ("rate-limit", True))
      , ("log-level",           ("log-level", True))
      , ("cors-origins",        ("cors-origins", True))
      , ("password-min-length", ("password-min-length", True))
      , ("max-bitrate",         ("max-bitrate", True))
      , ("hwaccel",             ("hwaccel", True))
      , ("watch",               ("watch", False))
      , ("follow-symlinks",     ("follow-symlinks", False))
      , ("tls-cert",            ("tls-cert", True))
      , ("tls-key",             ("tls-key", True))
      ]

-- | ASCII usage text printed by @--help@.
cliUsage :: String
cliUsage = unlines
  [ "usage: fms [options]"
  , ""
  , "options:"
  , "  --port N                HTTP port (env MEDIA_SERVER_PORT)"
  , "  --library-path PATH     media library root (env MEDIA_LIBRARY_PATH)"
  , "  --state-dir PATH        state directory (env FMS_STATE_DIR, default ./var)"
  , "  --require-auth[=BOOL]   require auth token on all non-open paths (env FMS_REQUIRE_AUTH)"
  , "  --server-name NAME      display name (env FMS_SERVER_NAME)"
  , "  --tmdb-api-key KEY      TMDB API key (env MEDIA_TMDB_API_KEY)"
  , "  --rate-limit N          requests/min per IP (env FMS_RATE_LIMIT, 0 = off)"
  , "  (env FMS_RATE_LIMIT_AUTH  POST /AuthenticateByName requests/min per IP, default 10, 0 = off)"
  , "  --log-level LEVEL       debug|info|warn|error (env FMS_LOG_LEVEL)"
  , "  --cors-origins LIST     comma-separated origins (env FMS_CORS_ORIGINS)"
  , "  --no-security-headers   disable X-Content-Type-Options etc. (env FMS_SECURITY_HEADERS)"
  , "  --password-min-length N minimum password length (env FMS_PASSWORD_MIN_LENGTH)"
  , "  --max-bitrate N         transcode bitrate cap in bps (env FMS_MAX_BITRATE)"
  , "  --hwaccel NAME          none|vaapi|nvenc|qsv|videotoolbox (env FMS_HWACCEL)"
  , "  --watch[=BOOL]          inotify auto-rescan (env FMS_WATCH, Linux only)"
  , "  --follow-symlinks[=BOOL] scanner symlink policy (env FMS_FOLLOW_SYMLINKS)"
  , "  --tls-cert PATH         TLS certificate PEM, enables HTTPS (env FMS_TLS_CERT)"
  , "  --tls-key PATH          TLS private key PEM (env FMS_TLS_KEY; both required)"
  , "  --help                  show this help and exit"
  , "  --version               show name and version and exit"
  , ""
  , "precedence: CLI flags > environment variables > stateDir/config.json > defaults"
  ]
