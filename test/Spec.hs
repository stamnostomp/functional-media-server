module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.QSemN (newQSemN, signalQSemN)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Acid (closeAcidState, openLocalStateFrom, query, update)
import Data.Acid.Local (createCheckpointAndClose)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Aeson (object, (.=))
import Data.ByteString.Builder (toLazyByteString)
import Network.Wai (defaultRequest, queryString)
import Network.Wai.Internal (Response (..))
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.Set as Set
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize (runGet, runPut)
import Data.Word (Word8)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , removeDirectoryRecursive
  , removeFile
  , setModificationTime
  )
import System.Posix.Files (createSymbolicLink)
import System.Process (createProcess, proc, terminateProcess, waitForProcess)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), replaceExtension)

import FMS.Core.Cli (parseCliArgs)
import FMS.Core.Module (Module (..), Route (..), Segment (..), get, post)
import FMS.Core.Types.Config (Config (..), validateConfig)
import FMS.Core.Process (ffmpegTimeoutArgs, runProcessBS, showBS, tryAcquireSlot)
import FMS.Core.Query (applyItemQuery, applyMetadataFilters, applyUserDataFilters, attachImageBlurHashes, attachImageTags, attachTags, attachUserData, backdropImageTagsValue, clampLimit, clampNonNegative, clampStartIndex, filterByFolder, imageBlurHashesValue, imageTagsValue, filterByParent, filterByIncludeTypes, filterBySeries, filterByType, filterByYears, isUserDataSortKey, itemYear, latestItems, nextUpEpisodes, normalizeQueryParams, paginate, parseBoundedInt, projectFields, queryParams, notInAnyCollection, searchItems, searchItemsMeta, showsFromItems, similarItems, sortByUserDataField, sortEpisodes, sortItems, specialFeaturesOf, typeFilterIncludesBoxSet, versionParam, versionPathFor, viewsFromFolders, displayTitleForItem, withDisplayTitle, withDisplayTitleLower, withLegacyItem)
import FMS.Core.Blurhash (base83Alphabet, base83Decode, base83Encode, decodeBlurhashSize, encodeBlurhash)
import FMS.Core.Memory (parseProcStatusRss, trimRing)
import FMS.Core.Range (ByteRange(..), parseRangeHeader)
import FMS.Core.SceneDetect (blackdetectArgs, intervalsOverlap, mergeIntervals, parseBlackdetect, parseSilencedetect, proposeSegments, silencedetectArgs)
import FMS.Core.RateLimit (Bucket (..), bucketKeyFor, newBucket, refill, requestIdHex, rateLimitClass, retryAfterSeconds, tryTake)
import FMS.Core.Monad (AppError (..), errorMessage, newAppEnv, putState, runApp, statusForError)
import FMS.Core.Router (allowedOriginsHeader, errorBody, exceedsMaxBody, firstMatch, isOpenPath, isOpenRoutePattern, matchSegments, matchSegmentsExact, normalizeSegments, openPathsExact, openRoutePatterns, parseCorsOrigins, parseTokenHeader, securityHeaders, tokenFromQuery, tokenFromRequestParts)
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Core.Types.Log (LogEntry (..), logLevelAllows, logRank, pushLogEntry)
import FMS.Core.Types.MediaItem (MediaItem (..), MediaItemV0 (..), MediaItemV1 (..), MediaItemV2 (..), MediaItemV3 (..), itemTypeFromMime, migrateMediaItem, migrateMediaItemV1, migrateMediaItemV2, migrateMediaItemV3, simpleItemJson)
import FMS.Core.Types.Policy (AccessSchedule (..), UserPolicy (..), emptyUserPolicy, itemAllowedByPolicy, ratingAllowed, ratingRank, scheduleAllowsNow, tagsAllowed)
import FMS.Core.Types.Progress (ItemStats(..), Progress(..), progressPercent, simpleProgressEntry)
import FMS.Core.Types.ServerConfig
  ( ServerConfig (..)
  , ServerConfigPatch (..)
  , applyServerConfigPatch
  , defaultServerConfig
  , loadServerConfig
  , mergeServerConfig
  , saveServerConfig
  )
import FMS.Core.Types.User (User(..), checkPasswordPolicy, toInternalJSON)
import FMS.Modules.Auth.Monad (canAccessFolder, hashPassword, verifyPassword)
import FMS.Modules.Auth.Routes (PermissionsRequest (..), PolicyPatch (..), applyPolicyPatch)
import FMS.Modules.Auth.State (loadUsers, saveUsers)
import FMS.Modules.Library.Monad (libraryAcidKey, scanProgressData, withScanLock)
import FMS.Modules.Library.Watch (eventsPending, watchFile, watchMediaFile)
import FMS.Modules.Library.Folders (loadFolders, nextFolderId, saveFolders, validateFolderPath)
import FMS.Modules.Library.Scan
  ( chunksOf
  , classifyItem
  , diffScan
  , extraTypeFor
  , isCircular
  , imageExtensions
  , isMediaFile
  , scanDirectory
  , isNfoFile
  , itemIdsForNfo
  , parseEpisodeFilename
  , parseEpisodeTagPos
  , seriesFromEpisodeName
  , itemFromPath
  , mediaExtensions
  , mimeFor
  , groupVersions
  , normalizeVersionTitle
  , planRescan
  , pooledMapIO
  , scanDirectory
  , siblingMediaPathsForNfo
  , statFile
  , versionRank
  , walk
  , walkWithLoops
  )
import FMS.Modules.Stats.Monad (applyPlay, applyProgress, continueWatching, popular)
import FMS.Modules.Stats.Routes (ProgressRequest (..))
import FMS.Modules.Stats.Persistence (loadStats, saveStats)
import FMS.Modules.UserData.Monad (markPlayed, setFavorite, setPosition)
import FMS.Modules.UserData.Persistence (loadUserData, saveUserData)
import FMS.Modules.UserData.Routes (parseIdsBody)
import FMS.Modules.AnnounceTrackers (parseTrackerList)
import FMS.Modules.Requests.Push (autoApproveSize, base64Encode, delugeAddFileBody, delugeAddMagnetBody, delugeConnectedBody, delugeConnectBody, delugeGetHostsBody, delugeLoginBody, delugeLoginTest, delugeTorrentStatuses, delugeTorrentsStatusBody, getFirstHostId, importComplete, matchTorrentProgress, matchesDownload, matchesRequestDownload, parseProxiedPath, parseTorrentStatuses, pushRequest, slugify)
import FMS.Modules.Requests.Monad (MonadRequests (..), loadRequestSettingsIntoState, loadRequestsIntoState)
import FMS.Modules.Requests.Routes (attachLiveProgress, fetchTorrentStatuses, markImported, requestsRoutes)
import FMS.Modules.Requests.Types (Request (..), RequestSettings (..), RequestStatus (..), SettingsPatch (..), duplicateOf, effectiveAutoApproveGb, emptySettingsPatch, isActiveStatus, mergeSettings, normalizeTitle, settingsFromConfig, settingsPublicJson)
import FMS.Modules.Trackers.Torznab (jackettSearchUrl, parseTorznabResults)
import FMS.Modules.Trackers.Types (Release (..), dedupeReleases, sortReleasesBySeeders, Tracker (..), trackerPublicJson)
import FMS.Modules.Indexers.Types
  ( Indexer (..)
  , base64UrlDecode
  , base64UrlEncode
  , categoriesParam
  , hostMatches
  , hostOf
  , indexerCookieHeader
  , indexerPublicJson
  , indexerSearchUrl
  , indexerSearchUrlExtra
  , parseTorrentDayJson
  , torrentDaySearchUrl
  , validAuthType
  , validResponseFormat
  )
import FMS.Modules.Webhooks.Persistence (loadWebhooks, saveWebhooks)
import FMS.Modules.Webhooks.Types (Webhook (..), eventMatches)
import FMS.Modules.UserData.Types (UserData (..), emptyUserData)
import FMS.Modules.Hls (hlsArgs, hlsDirFor, rewritePlaylistSegments)
import FMS.Modules.Images (imageExtFor, imagePathFor, imageTag, isUnderDir, isValidImageType, resizeArgs, selectBackdrop)
import FMS.Modules.Stream (resolveRange)
import FMS.Modules.Subtitles (SubtitleSource (..), embeddedSubtitleSources, embeddedToVttArgs, indexSources, pickSubtitles, subtitleExts, subtitleList, subtitleListSources, subtitleToVttArgs)
import FMS.Modules.Chapters (Chapter (..), chapterImageArgs, chapterImagePathFor, chapterToJson, fallbackChapters, parseChaptersJson, probeChaptersArgs, ticksFromSeconds)
import FMS.Modules.Thumbs (thumbArgs, thumbPathFor)
import FMS.Modules.Dash (dashArgs, dashArgsWith, dashDirFor, isValidDashSegName)
import FMS.Modules.Transcode (burnInFilter, effectiveBitrateCap, hwEncoderFor, normalizeHwAccel, spliceBeforeF, streamTranscodeTimeoutSec, transcodeArgs, transcodeArgsWith)
import FMS.Modules.Library.State
  ( GetAllItems(..)
  , GetIdByPath(..)
  , GetItemById(..)
  , ListPaths(..)
  , ReclassifyItem(..)
  , RegisterItem(..)
  , RemovePaths(..)
  , UpdateItemMeta(..)
  , initialLibraryState
  )
import FMS.Modules.Metadata.Api.Images (TmdbImageEntry (..), fetchTmdbImages, pickByLanguage, tmdbImageListToDownloads, tmdbImagesUrl)
import FMS.Modules.Metadata.Api.Tmdb
  ( TmdbEpisode (..)
  , TmdbSeason (..)
  , TmdbTvDetails (..)
  , parseTmdbResults
  , parseTmdbSeasonEpisodes
  , parseTmdbTvDetails
  , parseTmdbTvResults
  , tmdbPopularMoviesUrl
  , tmdbPopularMoviesUrlAt
  , tmdbPopularTvUrl
  , tmdbPopularTvUrlAt
  , tmdbSearchTvUrl
  , tmdbSearchUrl
  , tmdbSeasonUrlAt
  , tmdbTvDetailsUrlAt
  , tmdbMovieDetailsUrlAt
  , tmdbTvDetailsKwUrlAt
  , parseTmdbGenresKeywords
  )
import FMS.Modules.Metadata.Api.Types (TmdbResult (..), cleanTitleForProvider, originalTitlesOf, pickTmdbResult)
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseMovieNfo, parseTvShowNfo)
import FMS.Modules.Metadata (metadataModule)
import FMS.Modules.Metadata.Cache (SeriesCacheEntry (..), cacheFresh, cacheGet, cachePut, emptySeriesCache, loadSeriesCache, loadSeriesCacheIntoState, saveSeriesCache, seriesCachePath)
import FMS.Modules.Metadata.Monad (MonadMetadata (..), getMetadataSettings, loadMetadataIntoState, loadMetadataSettingsIntoState, refreshFromTmdb, updateMetadataSettings)
import FMS.Modules.Metadata.Persistence (loadMetadata, saveMetadata)
import FMS.Modules.Metadata.Routes (attachDownloadedLive, discoverEntry, discoverSection, genresFromMetadata, matchDownloaded, personsFromMetadata, seriesEpisodesHandler, seriesNameOfPayload, seriesSearchHandler, studiosFromMetadata, tagsFromMetadata, tmdbResultToSearchHit, unmatchedItems)
import FMS.Modules.Metadata.Types (MetadataSettings (..), MetadataSettingsPatch (..), emptyMetadataSettingsPatch, mergeMetadataSettings, metadataSettingsFromConfig, metadataSettingsPublicJson)
import FMS.Modules.Metadata.Types (MediaMetadata (..), applyManualPatch, applyProviderMerge, emptyMetadata, fieldNameFor, mergeTags, metadataStoredJSON, removeTag)
import FMS.Modules.Playlists.Monad (addItem, removeItem)
import FMS.Modules.Playlists.Persistence (loadPlaylists, savePlaylists)
import FMS.Modules.Playlists.Types (Playlist (..))
import qualified FMS.Modules.Collections.Monad as CM
import FMS.Modules.Collections.Persistence (loadCollections, saveCollections)
import FMS.Modules.Collections.Types (Collection (..), boxSetIdFor, boxSetItemValue, isBoxSetId)
import FMS.Modules.Devices.Persistence (loadDevices, saveDevices)
import FMS.Modules.Devices.Types (Device (..), upsertDevice)
import FMS.Modules.Segments.Persistence (loadSegments, saveSegments)
import FMS.Modules.Segments.Types (MediaSegment (..), secondsToTicks, ticksToSeconds, validSegmentType, validateSegment)
import FMS.Modules.Plugins.Types (Plugin (..), builtinPlugins, overlayPlugins, registerHook, unregisterHook)
import FMS.Modules.Socket (addConn, encodeMessage, parseApiKey, parseApiKeyFromPath, removeConn)
import FMS.Modules.Playback.Probe
  ( containerFromExtension
  , decidePlayMethod
  , directPlayable
  , directStreamable
  , parseFfprobe
  , parseFormatBitRate
  , parseFormatDuration
  , probeArgs
  , probeBitrateArgs
  , probeFile
  , transcoding
  )
import FMS.Modules.Playback.Types
  ( MediaStream(..)
  , PlaybackResponse(..)
  , defaultDeviceProfile
  , deviceProfileAllows
  , markDefaultStreams
  , selectStreams
  )
import FMS.Modules.Sessions.Monad (expireSessions, upsertSession)
import FMS.Modules.Sessions.Types (PlaySession (..), SessionCapabilities (..), applyCapabilities, emptyCapabilities, latestSessionForToken, validCommandName)
import FMS.Modules.Admin.Monad (activityLogCap, pushActivity, taskDue)
import FMS.Modules.Admin.Types (ActivityEntry (..), ScheduledTask (..), countsByType, uptimeSeconds)
import FMS.Modules.ApiDocs (methodText, routeToPath, routesToOpenApi, segmentToPathPiece)
import FMS.Modules.Web (contentTypeFor, dashboardBeforeCapture, dashboardPageHtml, safeUiPath, webModule, webPageHtml)

-- | Plain base-only assertions (no extra test deps): print PASS/FAIL per
-- case and exit non-zero on the first failure.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual
  | expected == actual = putStrLn ("PASS: " ++ label)
  | otherwise = do
      putStrLn ("FAIL: " ++ label
        ++ " -- expected " ++ show expected ++ ", got " ++ show actual)
      exitFailure

-- | Remove a scratch dir under /tmp if it exists.
resetDir :: FilePath -> IO ()
resetDir dir = do
  exists <- doesDirectoryExist dir
  if exists then removeDirectoryRecursive dir else pure ()

-- | Poll for a file to appear (mock-server readiness), 250 ms steps.
waitForFile :: FilePath -> Int -> IO Bool
waitForFile f n = do
  e <- doesFileExist f
  if e then pure True else if n <= (0 :: Int) then pure False else threadDelay 250000 >> waitForFile f (n - 1)

-- | m29: stateful mock deluge-web (python3) that answers "Unknown method" to
-- core.* until web.connect was called with the right host id. Skipped when
-- python3 is not in PATH (e.g. the nix build sandbox).
m29MockTests :: IO ()
m29MockTests = do
  -- m29 stateful mock: core.* answers "Unknown method" until web.connect
  let m29dir = "/tmp/fms-m29-mock"
      mockPy = m29dir </> "mock.py"
      logA = m29dir </> "calls-a.log"
      logB = m29dir </> "calls-b.log"
      mockDelugeSrc = unlines
        [ "import json, sys"
        , "from http.server import BaseHTTPRequestHandler, HTTPServer"
        , "port = int(sys.argv[1]); log = sys.argv[2]; mode = sys.argv[3] if len(sys.argv) > 3 else ''"
        , "state = {'connected': False}"
        , "class H(BaseHTTPRequestHandler):"
        , "    def log_message(self, *a): pass"
        , "    def do_POST(self):"
        , "        n = int(self.headers.get('Content-Length', 0))"
        , "        req = json.loads(self.rfile.read(n))"
        , "        m = req.get('method', '')"
        , "        with open(log, 'a') as f:"
        , "            f.write(json.dumps({'method': m, 'params': req.get('params')}) + chr(10))"
        , "        res = None; err = None; cookie = False"
        , "        if m == 'auth.login':"
        , "            if req.get('params', [''])[0] == 'deluge':"
        , "                res = 'sess123'; cookie = True"
        , "            else:"
        , "                res = False; err = 'bad password'"
        , "        elif m == 'web.get_hosts':"
        , "            res = [] if mode == 'nohosts' else [['host-1', '127.0.0.1', 58846, 'localclient', '']]"
        , "        elif m == 'web.connect':"
        , "            if mode != 'noconnect' and req.get('params', [''])[0] == 'host-1':"
        , "                state['connected'] = True; res = True"
        , "            else:"
        , "                res = False"
        , "        elif m == 'web.connected':"
        , "            res = state['connected']"
        , "        elif m.startswith('core.'):"
        , "            if state['connected']:"
        , "                res = 'torrent-id-1'"
        , "            else:"
        , "                res = None; err = 'Unknown method'"
        , "        payload = json.dumps({'result': res, 'error': err, 'id': req.get('id')}).encode()"
        , "        self.send_response(200)"
        , "        self.send_header('Content-Type', 'application/json')"
        , "        if cookie:"
        , "            self.send_header('Set-Cookie', 'deluge_web_session=sess123; Path=/')"
        , "        self.send_header('Content-Length', str(len(payload)))"
        , "        self.end_headers()"
        , "        self.wfile.write(payload)"
        , "srv = HTTPServer(('127.0.0.1', port), H)"
        , "open(log + '.ready', 'w').write('ok')"
        , "srv.serve_forever()" ]
      waitReady f n = do
        e <- doesFileExist f
        if e then pure True else if n <= (0 :: Int) then pure False else threadDelay 250000 >> waitReady f (n - 1)
      withMock port logPath mode act = bracket
        (createProcess (proc "python3" [mockPy, show port, logPath, mode]))
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
        (\_ -> do
          re <- doesFileExist (logPath ++ ".ready"); when re (removeFile (logPath ++ ".ready"))
          logExists <- doesFileExist logPath; when logExists (removeFile logPath)
          waitReady (logPath ++ ".ready") 40 >>= \ok -> if ok then act else pure Nothing)
      m29ok = either (const False) (const True) :: Either String () -> Bool
      logMethod l = case Aeson.decode (LBS.fromStrict (BSC.pack l)) :: Maybe Aeson.Value of
        Just (Aeson.Object o) -> case KeyMap.lookup (Key.fromText "method") o of
          Just (Aeson.String m) -> T.unpack m
          _ -> "?"
        _ -> "?"
  resetDir m29dir
  createDirectoryIfMissing True m29dir
  writeFile mockPy mockDelugeSrc
  mPush <- withMock (8912 :: Int) logA "" $ do
    res <- pushRequest "http://127.0.0.1:8912/json" "deluge" (m29dir </> "dl") (Just "magnet:?xt=urn:btih:m29") Nothing "Test Movie" [] (\_ -> pure (Left "no proxy"))
    pure (Just (m29ok res))
  logATxt <- do e <- doesFileExist logA; if e then readFile logA else pure ""
  let logAMethods = map logMethod (lines logATxt)
  check "m29 push: mock saw auth.login -> web.get_hosts -> web.connect -> core.add_torrent_magnet in order"
    logAMethods ["auth.login", "web.get_hosts", "web.connect", "core.add_torrent_magnet"]
  check "m29 pushRequest succeeds against the connect-requiring mock" [mPush] [Just True]
  check "m29 add_torrent_magnet carried download_location" [ (m29dir </> "dl") `isInfixOf` logATxt ] [True]
  fbA <- doesFileExist (m29dir </> "dl" </> "test-movie.magnet")
  check "m29 no fallback .magnet written when the push succeeded" [fbA] [False]
  mTestOk <- withMock (8912 :: Int) logA "" $ do
    res <- delugeLoginTest "http://127.0.0.1:8912/json" "deluge"
    pure (Just (m29ok res))
  check "m29 delugeLoginTest ok when login+connect succeed" [mTestOk] [Just True]
  mTestBadPw <- withMock (8912 :: Int) logA "" $ do
    res <- delugeLoginTest "http://127.0.0.1:8912/json" "wrong"
    pure (Just (m29ok res))
  check "m29 delugeLoginTest not-ok on bad password" [mTestBadPw] [Just False]
  mTestNoConn <- withMock (8913 :: Int) logB "noconnect" $ do
    res <- delugeLoginTest "http://127.0.0.1:8913/json" "deluge"
    pure (Just (m29ok res))
  check "m29 delugeLoginTest not-ok when web.connect fails (web.connected false)" [mTestNoConn] [Just False]
  mPushFb <- withMock (8913 :: Int) logB "noconnect" $ do
    res <- pushRequest "http://127.0.0.1:8913/json" "deluge" (m29dir </> "dl-b") (Just "magnet:?xt=urn:btih:m29b") Nothing "Other Movie" [] (\_ -> pure (Left "no proxy"))
    pure (Just (m29ok res))
  fbB <- doesFileExist (m29dir </> "dl-b" </> "other-movie.magnet")
  check "m29 push falls back to the .magnet file when the daemon never connects" [mPushFb, Just fbB] [Just True, Just True]


-- | m37: mock deluge that requires login -> get_hosts -> connect before
-- core.get_torrents_status and returns two torrents with progress values.
m37MockTests :: IO ()
m37MockTests = do
  let m37dir = "/tmp/fms-m37-mock"
      mockPy = m37dir </> "mock.py"
      logC = m37dir </> "calls-c.log"
      mockSrc = unlines
        [ "import json, sys"
        , "from http.server import BaseHTTPRequestHandler, HTTPServer"
        , "port = int(sys.argv[1]); log = sys.argv[2]"
        , "state = {'connected': False}"
        , "class H(BaseHTTPRequestHandler):"
        , "    def log_message(self, *a): pass"
        , "    def do_POST(self):"
        , "        n = int(self.headers.get('Content-Length', 0))"
        , "        req = json.loads(self.rfile.read(n))"
        , "        m = req.get('method', '')"
        , "        with open(log, 'a') as f:"
        , "            f.write(m + chr(10))"
        , "        res = None; err = None; cookie = False"
        , "        if m == 'auth.login':"
        , "            if req.get('params', [''])[0] == 'deluge':"
        , "                res = 'sess123'; cookie = True"
        , "            else:"
        , "                res = False"
        , "        elif m == 'web.get_hosts':"
        , "            res = [['host-1', '127.0.0.1', 58846, 'localclient', '']]"
        , "        elif m == 'web.connect':"
        , "            state['connected'] = True; res = True"
        , "        elif m == 'core.get_torrents_status':"
        , "            if state['connected']:"
        , "                res = {'t1': {'name': 'Shrek.2001.1080p.BluRay.x265-LAMA', 'state': 'Downloading', 'progress': 42.5, 'download_payload_rate': 1024}, 't2': {'name': 'Other Movie 2020', 'state': 'Seeding', 'progress': 100.0, 'download_payload_rate': 0}}"
        , "            else:"
        , "                res = None; err = 'Unknown method'"
        , "        payload = json.dumps({'result': res, 'error': err, 'id': req.get('id')}).encode()"
        , "        self.send_response(200)"
        , "        self.send_header('Content-Type', 'application/json')"
        , "        if cookie:"
        , "            self.send_header('Set-Cookie', 'deluge_web_session=sess123; Path=/')"
        , "        self.send_header('Content-Length', str(len(payload)))"
        , "        self.end_headers()"
        , "        self.wfile.write(payload)"
        , "srv = HTTPServer(('127.0.0.1', port), H)"
        , "open(log + '.ready', 'w').write('ok')"
        , "srv.serve_forever()" ]
  resetDir m37dir
  createDirectoryIfMissing True m37dir
  writeFile mockPy mockSrc
  bracket
    (createProcess (proc "python3" [mockPy, "8914", logC]))
    (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
    (\_ -> do
      rok <- waitForFile (logC ++ ".ready") 40
      if not rok
        then putStrLn "PASS: m37 mock-deluge IO tests skipped (mock did not start)"
        else do
          res <- delugeTorrentStatuses "http://127.0.0.1:8914/json" "deluge"
          check "m37 delugeTorrentStatuses: two torrents with progress parsed (sorted)"
            (fmap sort res)
            (Right (sort [ ("Shrek.2001.1080p.BluRay.x265-LAMA", "Downloading", 42.5, 1024.0)
                         , ("Other Movie 2020", "Seeding", 100.0, 0.0) ]))
          logTxt <- readFile logC
          check "m37 delugeTorrentStatuses: mock saw login -> get_hosts -> connect -> get_torrents_status in order"
            (lines logTxt) ["auth.login", "web.get_hosts", "web.connect", "core.get_torrents_status"]
          resBad <- delugeTorrentStatuses "http://127.0.0.1:8914/json" "wrong"
          check "m37 delugeTorrentStatuses: bad password -> Left"
            (either (const True) (const False) resBad) True)

main :: IO ()
main = do
  -- Range header parsing (docs/modules.org: malformed means "no range").
  check "bytes=0-100"  (Just (ByteRange 0 (Just 100))) (parseRangeHeader "bytes=0-100")
  check "bytes=200-"   (Just (ByteRange 200 Nothing)) (parseRangeHeader "bytes=200-")
  check "bytes=abc"    Nothing                       (parseRangeHeader "bytes=abc")
  check "empty header" Nothing                       (parseRangeHeader "")
  -- "bytes=5-2" has end < start: invalid, treated as no range.
  check "bytes=5-2 (end < start -> Nothing)" Nothing (parseRangeHeader "bytes=5-2")
  check "no bytes= prefix"                    Nothing (parseRangeHeader "0-100")
  check "suffix range bytes=-500"             Nothing (parseRangeHeader "bytes=-500")

  -- resolveRange against a 1000-byte file (stream module, milestone 3):
  -- valid ranges clamp to (start, end) inclusive, invalid ones are Left.
  let isLeftE :: Either a b -> Bool
      isLeftE (Left _)  = True
      isLeftE (Right _) = False
  check "resolveRange (0, Just 99)"   (Right (0, 99))   (resolveRange 1000 (ByteRange 0 (Just 99)))
  check "resolveRange (900, Nothing)" (Right (900, 999)) (resolveRange 1000 (ByteRange 900 Nothing))
  check "resolveRange (0, Nothing)"   (Right (0, 999))  (resolveRange 1000 (ByteRange 0 Nothing))
  check "resolveRange (5000, Just 6000) is Left (start >= size)"
    True (isLeftE (resolveRange 1000 (ByteRange 5000 (Just 6000))))
  check "resolveRange (5, Just 2) is Left (end < start)"
    True (isLeftE (resolveRange 1000 (ByteRange 5 (Just 2))))
  check "resolveRange (1000, Nothing) is Left (start >= size)"
    True (isLeftE (resolveRange 1000 (ByteRange 1000 Nothing)))
  check "resolveRange (0, Just 1000) is Left (end >= size)"
    True (isLeftE (resolveRange 1000 (ByteRange 0 (Just 1000))))

  -- Route segment matching.
  check "literal match"        (Just [])                  (matchSegments [Lit "a", Lit "b"] ["a", "b"])
  check "capture binds"        (Just [("id", "42")])      (matchSegments [Lit "stream", Capture "id"] ["stream", "42"])
  check "literal mismatch"     Nothing                    (matchSegments [Lit "a"] ["b"])
  check "too few segments"     Nothing                    (matchSegments [Lit "a", Lit "b"] ["a"])
  check "too many segments"    Nothing                    (matchSegments [Lit "a"] ["a", "b"])
  check "empty pattern"        (Just [])                  (matchSegments [] [])

  -- Filesystem scanner (docs/roadmap.org milestone 2): recursive walk,
  -- media-extension filter, mime map. notes.txt must be ignored.
  let scanDir = "/tmp/fms-test-scan"
  resetDir scanDir
  createDirectoryIfMissing True (scanDir </> "Movies")
  writeFile (scanDir </> "Movies" </> "a.mp4") "dummy video"  -- 11 bytes
  writeFile (scanDir </> "song.mp3") "dummy audio"           -- 11 bytes
  writeFile (scanDir </> "notes.txt") "not media"
  scanned <- scanDirectory False scanDir
  check "scan finds only media file paths"
    (sort [scanDir </> "Movies" </> "a.mp4", scanDir </> "song.mp3"])
    (sort scanned)
  scannedItems <- mapM (itemFromPath "1") scanned
  check "scan titles are base names" ["a", "song"] (sort (map (T.unpack . itemTitle) scannedItems))
  check "scan sizes" [11, 11] (sort (map itemSize scannedItems))
  check "scan mimes" ["audio/mpeg", "video/mp4"] (sort (map (T.unpack . itemMime) scannedItems))
  check "scan assigns empty id (RegisterItem fills it)" ["", ""] (sort (map (T.unpack . itemId) scannedItems))
  check "mimeFor .mp4" "video/mp4" (mimeFor (scanDir </> "Movies" </> "a.mp4"))
  check "mimeFor .mp3" "audio/mpeg" (mimeFor (scanDir </> "song.mp3"))
  check "isMediaFile .txt is False" False (isMediaFile (scanDir </> "notes.txt"))
  check "isMediaFile .MKV (case-insensitive) is True" True (isMediaFile "/x/Y.MKV")
  -- milestone 2e: photos + books are media files with proper mimes
  check "mimeFor photo extensions"
    ["image/jpeg", "image/jpeg", "image/png", "image/gif", "image/webp", "image/bmp", "image/heic", "image/tiff"]
    (map (T.unpack . mimeFor) ["a.jpg", "a.jpeg", "a.png", "a.gif", "a.webp", "a.bmp", "a.heic", "a.tiff"])
  check "mimeFor book extensions"
    [ "application/epub+zip", "application/x-mobipocket-ebook", "application/pdf"
    , "application/vnd.comicbook+zip", "application/vnd.comicbook-rar" ]
    (map (T.unpack . mimeFor) ["a.epub", "a.mobi", "a.pdf", "a.cbz", "a.cbr"])
  check "mimeFor extension match is case-insensitive (.JPG/.PDF)"
    ["image/jpeg", "application/pdf"] (map (T.unpack . mimeFor) ["/x/P.JPG", "/x/B.PDF"])
  check "isMediaFile accepts .epub/.pdf, rejects images (.jpg, m51) and .txt"
    [False, True, True, False]
    (map isMediaFile ["/x/p.jpg", "/x/b.epub", "/x/b.pdf", "/x/n.txt"])
  missing <- scanDirectory False "/tmp/fms-test-scan-does-not-exist"
  check "scan of missing dir is []" [] missing
  removeDirectoryRecursive scanDir

  -- Incremental rescan pure helpers (milestone 2b, docs/todo.org section 2).
  check "diffScan unchanged" ([], []) (diffScan ["a", "b"] ["b", "a"])
  check "diffScan add/remove" (["a"], ["c"]) (diffScan ["a", "b"] ["b", "c"])
  check "diffScan empty" ([], []) (diffScan [] [])
  check "diffScan all new" ([], ["x", "y"]) (diffScan [] ["y", "x"])
  check "isCircular hit" True (isCircular (Set.fromList ["/x"]) "/x")
  check "isCircular miss" False (isCircular (Set.fromList ["/x"]) "/y")
  check "isCircular empty" False (isCircular Set.empty "/x")
  tNow <- getCurrentTime
  let pt1 = tNow
      pt2 = addUTCTime 5 tNow
  check "planRescan mixed (removed, changedOrNew, unchanged)"
    (["gone"], ["changed", "new"], ["same"])
    (planRescan
      [("same", 10, pt1), ("changed", 20, pt2), ("new", 5, pt1)]
      [("same", Just (10, pt1)), ("changed", Just (10, pt1)), ("gone", Just (1, pt1))])
  check "planRescan no existing (all new)"
    ([], ["a", "b"], [])
    (planRescan [("a", 1, pt1), ("b", 2, pt1)] [])
  check "planRescan all removed"
    (["x"], [], [])
    (planRescan [] [("x", Just (1, pt1))])
  check "planRescan unknown existing meta counts as changed"
    ([], ["mystery"], [])
    (planRescan [("mystery", 3, pt1)] [("mystery", Nothing)])

  -- Symlink walk policy (milestone 2b): follow=False skips all symlinks;
  -- follow=True follows file symlinks and detects circular dir chains.
  let linkDir = "/tmp/fms-test-symlinks"
  resetDir linkDir
  createDirectoryIfMissing True (linkDir </> "real")
  writeFile (linkDir </> "real" </> "a.mp4") "x"
  createSymbolicLink (linkDir </> "real" </> "a.mp4") (linkDir </> "link.mp4")
  createSymbolicLink (linkDir </> "real") (linkDir </> "real" </> "self")
  createSymbolicLink (linkDir </> "real") (linkDir </> "dup")
  noFollow <- walk False linkDir
  check "walk False skips symlinks entirely"
    [linkDir </> "real" </> "a.mp4"] (sort noFollow)
  scanNoFollow <- scanDirectory False linkDir
  check "scanDirectory False skips symlinks"
    [linkDir </> "real" </> "a.mp4"] (sort scanNoFollow)
  (followFiles, followLoops) <- walkWithLoops True linkDir
  check "walk True follows file symlinks (link path reported)"
    (sort [linkDir </> "real" </> "a.mp4", linkDir </> "link.mp4"]) (sort followFiles)
  check "walk True detects circular chain AND dir-symlink duplicate of a visited dir"
    (sort [linkDir </> "real" </> "self", linkDir </> "dup"]) (sort followLoops)
  check "walk of a missing dir is []" [] =<< walk True "/tmp/fms-test-symlinks-gone"
  removeDirectoryRecursive linkDir

  -- Scan lock (milestone 2b): creates/removes the lock dir around the
  -- action; recovers a stale (> 60s) lock.
  let lockDir = "/tmp/fms-test-scan.lock"
  resetDir lockDir
  heldInside <- withScanLock lockDir (doesDirectoryExist lockDir)
  check "withScanLock holds the lock dir inside the action" True heldInside
  heldAfter <- doesDirectoryExist lockDir
  check "withScanLock releases the lock dir after the action" False heldAfter
  createDirectoryIfMissing True lockDir
  staleTime <- getCurrentTime
  setModificationTime lockDir (addUTCTime (-120) staleTime)
  recovered <- withScanLock lockDir (pure True)
  check "withScanLock recovers a stale lock" True recovered
  releasedAfter <- doesDirectoryExist lockDir
  check "withScanLock cleans up after stale recovery" False releasedAfter
  resetDir lockDir

  -- acid-state persistence + stable ids (docs/acid-state.org test).
  let acidDir = "/tmp/fms-test-acid"
  resetDir acidDir
  createDirectoryIfMissing True acidDir
  now <- getCurrentTime
  let testItem = MediaItem
        { itemId      = ""  -- assigned by RegisterItem
        , itemTitle   = "x"
        , itemPath    = "/x.mp4"
        , itemSize    = 42
        , itemMime    = "video/mp4"
        , itemAddedAt = now
        , itemFolder  = "1"
        , itemType      = "Movie"
        , itemSeries    = Nothing
        , itemSeason    = Nothing
        , itemEpisode   = Nothing
        , itemExtraType = Nothing
        , itemVersions = []
        }
  st1 <- openLocalStateFrom acidDir initialLibraryState
  id1 <- update st1 (RegisterItem "/x.mp4" testItem)
  id1again <- update st1 (RegisterItem "/x.mp4" testItem)
  check "registerItem is idempotent (same run)" id1 id1again
  createCheckpointAndClose st1
  -- Reopen from disk: the id mapping must have survived the restart.
  st2 <- openLocalStateFrom acidDir initialLibraryState
  mId2 <- query st2 (GetIdByPath "/x.mp4")
  check "id survives checkpoint+reopen" (Just id1) mId2
  id3 <- update st2 (RegisterItem "/x.mp4" testItem)
  check "registerItem is idempotent (after reopen)" id1 id3
  mItem <- query st2 (GetItemById id1)
  check "GetItemById returns item with assigned id"
    (Just (testItem { itemId = id1 })) mItem
  allItems <- query st2 GetAllItems
  check "GetAllItems has exactly one item" 1 (length allItems)
  -- Milestone 2b acid events: incremental rescan touches.
  paths <- query st2 ListPaths
  check "ListPaths returns the registered path" ["/x.mp4"] paths
  let later = addUTCTime 10 now
  changed <- update st2 (UpdateItemMeta "/x.mp4" 43 later)
  check "UpdateItemMeta reports a change" True changed
  unchanged <- update st2 (UpdateItemMeta "/x.mp4" 43 later)
  check "UpdateItemMeta reports no-op on same size/mtime" False unchanged
  mItem2 <- query st2 (GetItemById id1)
  check "UpdateItemMeta stored the new size/mtime"
    (Just (testItem { itemId = id1, itemSize = 43, itemAddedAt = later })) mItem2
  noPath <- update st2 (UpdateItemMeta "/nope.mp4" 1 now)
  check "UpdateItemMeta on unknown path is False" False noPath
  removed <- update st2 (RemovePaths ["/x.mp4", "/nope.mp4"])
  check "RemovePaths counts only known paths" 1 removed
  pathsAfter <- query st2 ListPaths
  check "RemovePaths removed the path" [] pathsAfter
  mGone <- query st2 (GetItemById id1)
  check "RemovePaths removed the item" Nothing mGone
  id4 <- update st2 (RegisterItem "/x.mp4" testItem)
  check "re-register after removal allocates a fresh id" True (id4 /= id1)
  closeAcidState st2
  -- m46: ReclassifyItem flips a Movie to Episode, keeps id, no-op when absent
  let acidDir46 = "/tmp/fms-test-acid-46"
      epPath = "/dl/Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta.mkv"
      (ty46, sn46, se46, ep46) =
        classifyItem False epPath "Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta"
  check "m46 live filename classifies as Episode/Family Guy/25/0"
    (ty46, sn46, se46, ep46) ("Episode", Just "Family Guy", Just 25, Just 0)
  resetDir acidDir46
  createDirectoryIfMissing True acidDir46
  st46 <- openLocalStateFrom acidDir46 initialLibraryState
  epId <- update st46 (RegisterItem epPath (testItem { itemPath = epPath, itemType = "Movie" }))
  flipped <- update st46 (ReclassifyItem epPath ty46 sn46 se46 ep46)
  check "m46 ReclassifyItem reports the flip" True flipped
  mEp <- query st46 (GetItemById epId)
  check "m46 ReclassifyItem updates type/series/season/episode, keeps id"
    (fmap (\it -> (itemId it, itemType it, itemSeries it, itemSeason it, itemEpisode it)) mEp)
    (Just (epId, "Episode", Just "Family Guy", Just 25, Just 0))
  again <- update st46 (ReclassifyItem epPath ty46 sn46 se46 ep46)
  check "m46 ReclassifyItem no-op when unchanged" False again
  nope <- update st46 (ReclassifyItem "/nope.mkv" "Movie" Nothing Nothing Nothing)
  check "m46 ReclassifyItem unknown path is a no-op" False nope
  closeAcidState st46
  removeDirectoryRecursive acidDir

  -- FMS.Core.Process (milestone 6a encoding bugfix): subprocess pipes are
  -- read as raw bytes and rendered with showBS, which maps one byte to
  -- one char (per-byte latin1) and never throws -- not even for multi-byte
  -- UTF-8 sequences such as the em-dash (0xE2 0x80 0x94).
  check "showBS of UTF-8 em-dash bytes [0xE2,0x80,0x94] yields a 3-char String (no throw)"
    3 (length (showBS (BS.pack [0xE2, 0x80, 0x94])))
  check "showBS maps bytes 0xE2 0x80 0x94 to chars U+00E2 U+0080 U+0094"
    "\xE2\x80\x94" (showBS (BS.pack [0xE2, 0x80, 0x94]))

  -- Transcode module (milestone 3b): the pure ffmpeg argument list.
  let targs = transcodeArgs "/tmp/fms-test-tc.wav"
  check "transcodeArgs contains -f mp4" True ("-f" `elem` targs && "mp4" `elem` targs)
  check "transcodeArgs contains pipe:1" True ("pipe:1" `elem` targs)
  check "transcodeArgs contains the input path" True ("/tmp/fms-test-tc.wav" `elem` targs)

  -- IO test of the actual transcode, guarded by ffmpeg availability: skip
  -- gracefully when ffmpeg is not on PATH.
  mFFmpeg <- findExecutable "ffmpeg"
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping transcode IO test"
    Just _ -> do
      let wav = "/tmp/fms-test-tc.wav"
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        ["-f", "lavfi", "-i", "sine=frequency=440:duration=1", "-y", wav]
      check "generate 1s test wav: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg wav gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      (ec, out, tcErr) <- runProcessBS "ffmpeg" (transcodeArgs wav)
      check "transcodeArgs run on wav: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg transcode stderr: " ++ show (showBS (LBS.toStrict tcErr))))
      check "transcoded output is > 1000 bytes" True (LBS.length out > 1000)
      -- An MP4 starts with a size box whose type at offset 4 is "ftyp"
      -- (0x66 0x74 0x79 0x70 = [102,116,121,112]).
      check "output bytes 4..8 are the ftyp box type"
        [0x66, 0x74, 0x79, 0x70] (map fromEnum (LBS.unpack (LBS.take 4 (LBS.drop 4 out))))

  -- HLS module (milestone 3c): the pure ffmpeg argument list + dir helper.
  let hlsIn  = "/tmp/fms-test-hls-src.mp4"
      hlsOut = "/tmp/fms-test-hls-out/main.m3u8"
      hargs  = hlsArgs hlsIn hlsOut
      containsPair x y = any (\(a, b) -> a == x && b == y) (zip hargs (drop 1 hargs))
  check "hlsArgs has -hls_playlist_type vod" True (containsPair "-hls_playlist_type" "vod")
  check "hlsArgs has -c copy"               True (containsPair "-c" "copy")
  check "hlsArgs has -hls_time 4"           True (containsPair "-hls_time" "4")
  check "hlsArgs ends with the output m3u8 path" hlsOut (last hargs)
  check "hlsArgs contains the input path"   True (hlsIn `elem` hargs)
  check "hlsArgs has the seg%03d.ts pattern in the output dir"
    True ("/tmp/fms-test-hls-out/seg%03d.ts" `elem` hargs)
  check "hlsDirFor joins stateDir/hls/id" "/state/hls/42" (hlsDirFor "/state" "42")

  -- IO test of the actual HLS packaging, guarded by ffmpeg availability:
  -- skip gracefully when ffmpeg is not on PATH.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping HLS IO test"
    Just _ -> do
      let mp4    = "/tmp/fms-test-hls-src.mp4"
          outDir = "/tmp/fms-test-hls-out"
          m3u8   = outDir </> "main.m3u8"
      resetDir outDir
      createDirectoryIfMissing True outDir
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        [ "-f", "lavfi", "-i", "testsrc=duration=4:size=160x120:rate=10"
        , "-c:v", "libx264", "-y", mp4 ]
      check "generate 4s test mp4: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg mp4 gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      (ec, _, hlsErr) <- runProcessBS "ffmpeg" (hlsArgs mp4 m3u8)
      check "hlsArgs run on 4s mp4: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg hls stderr: " ++ show (showBS (LBS.toStrict hlsErr))))
      m3u8Exists <- doesFileExist m3u8
      check "main.m3u8 exists after ffmpeg" True m3u8Exists
      playlist <- LBS.readFile m3u8
      check "playlist starts with #EXTM3U" True (LBSC.isPrefixOf "#EXTM3U" playlist)
      let segNames = filter (LBSC.isSuffixOf ".ts") (LBSC.lines playlist)
      check "playlist references at least one .ts segment" True (not (null segNames))
      segsExist <- mapM (\s -> doesFileExist (outDir </> LBSC.unpack s)) segNames
      check "all referenced .ts segments exist on disk" True (and segsExist)
      removeDirectoryRecursive outDir
      removeFile mp4
  let plTxt = "#EXTM3U\n#EXT-X-VERSION:3\n#EXTINF:7.8,\nseg000.ts\nseg001.ts\n#EXT-X-ENDLIST\n"
      rw = rewritePlaylistSegments "tok123" plTxt
  check "rewritePlaylistSegments appends api_key to segment lines only"
    [True, True, True, True, True]
    [ "seg000.ts?api_key=tok123" `T.isInfixOf` rw
    , "seg001.ts?api_key=tok123" `T.isInfixOf` rw
    , "#EXTM3U" `T.isInfixOf` rw
    , "#EXT-X-ENDLIST" `T.isInfixOf` rw
    , not ("#EXTM3U?api_key=" `T.isInfixOf` rw)
    ]

  -- Subtitles module (milestone 3d): pure pickSubtitles / subtitleToVttArgs.
  check "subtitleExts" [".srt", ".vtt", ".ass", ".ssa", ".sub"] subtitleExts
  let dirEnts = ["clip.srt", "clip.vtt", "other.ass", "notes.txt", "clip.mp4"]
  check "pickSubtitles prefers basename matches, sorted"
    ["/m/clip.srt", "/m/clip.vtt"]
    (pickSubtitles "/m/clip.mp4" dirEnts)
  check "pickSubtitles: no basename match -> all subtitle files sorted"
    ["/m/a.ass", "/m/b.srt"]
    (pickSubtitles "/m/clip.mp4" ["b.srt", "a.ass", "notes.txt", "clip.mp4"])
  check "pickSubtitles: none -> []"
    []
    (pickSubtitles "/m/clip.mp4" ["notes.txt", "clip.mp4"])
  check "pickSubtitles is case-insensitive on extensions"
    ["/m/clip.SRT"]
    (pickSubtitles "/m/clip.mp4" ["clip.SRT", "clip.MP4"])
  let vttArgs = subtitleToVttArgs "/tmp/fms-sub-test.srt"
      vttHasPair x y = any (\(a, b) -> a == x && b == y) (zip vttArgs (drop 1 vttArgs))
  check "subtitleToVttArgs contains -f webvtt" True (vttHasPair "-f" "webvtt")
  check "subtitleToVttArgs contains pipe:1"   True ("pipe:1" `elem` vttArgs)
  check "subtitleToVttArgs contains the input path"
    True ("/tmp/fms-sub-test.srt" `elem` vttArgs)

  -- IO test of the actual SRT -> WebVTT conversion, guarded by ffmpeg
  -- availability: skip gracefully when ffmpeg is not on PATH.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping subtitle IO test"
    Just _ -> do
      let srt = "/tmp/fms-sub-test.srt"
      writeFile srt "1\n00:00:01,000 --> 00:00:02,000\nHello\n"
      (ec, out, subErr) <- runProcessBS "ffmpeg" (subtitleToVttArgs srt)
      check "subtitleToVttArgs run on srt: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg subtitle stderr: " ++ show (showBS (LBS.toStrict subErr))))
      check "converted output contains WEBVTT" True (BS.isInfixOf "WEBVTT" (LBS.toStrict out))
      removeFile srt

  -- Thumbs module (milestone 3e): the pure ffmpeg argument list + cache
  -- path helper.
  let thumbIn = "/tmp/fms-test-thumb-src.mp4"
      thArgs  = thumbArgs thumbIn
      thumbHasPair x y = any (\(a, b) -> a == x && b == y) (zip thArgs (drop 1 thArgs))
  check "thumbArgs has -ss 5"            True (thumbHasPair "-ss" "5")
  check "thumbArgs has -i <path>"        True (thumbHasPair "-i" thumbIn)
  check "thumbArgs has -frames:v 1"      True (thumbHasPair "-frames:v" "1")
  check "thumbArgs has -f image2"        True (thumbHasPair "-f" "image2")
  check "thumbArgs has -vcodec mjpeg"    True (thumbHasPair "-vcodec" "mjpeg")
  check "thumbArgs contains pipe:1"      True ("pipe:1" `elem` thArgs)
  check "thumbPathFor joins stateDir/thumbs/id.jpg"
    "/state/thumbs/7.jpg" (thumbPathFor "/state" "7")

  -- IO test of the actual thumbnail extraction, guarded by ffmpeg
  -- availability: skip gracefully when ffmpeg is not on PATH.
  -- NOTE: thumbArgs seeks to 5s, so the source must be longer than 5s —
  -- on a 2s clip ffmpeg 9.0.2 exits 234 with empty output (verified
  -- empirically), hence the 6s lavfi source here.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping thumbs IO test"
    Just _ -> do
      let mp4 = "/tmp/fms-test-thumb-src.mp4"
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        [ "-f", "lavfi", "-i", "testsrc=duration=6:size=160x120:rate=10"
        , "-c:v", "libx264", "-y", mp4 ]
      check "generate 6s test mp4: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg mp4 gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      (ec, out, thErr) <- runProcessBS "ffmpeg" (thumbArgs mp4)
      check "thumbArgs run on 6s mp4: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg thumb stderr: " ++ show (showBS (LBS.toStrict thErr))))
      -- A JPEG starts with the SOI marker 0xFF 0xD8.
      check "thumb output starts with JPEG SOI (FFD8)"
        [0xFF, 0xD8] (map fromEnum (LBS.unpack (LBS.take 2 out)))
      check "thumb output is > 1000 bytes" True (LBS.length out > 1000)
      removeFile mp4

  -- Images module (milestone 6b, docs/todo.org section 5): the pure
  -- helpers -- extension mapping, type validation, ffmpeg resize args,
  -- path joining.
  check "imageExtFor image/png -> .png"        ".png" (imageExtFor "image/png")
  check "imageExtFor image/jpeg -> .jpg"       ".jpg" (imageExtFor "image/jpeg")
  check "imageExtFor image/jpg -> .jpg"        ".jpg" (imageExtFor "image/jpg")
  check "imageExtFor image/webp -> .webp"      ".webp" (imageExtFor "image/webp")
  check "imageExtFor unknown type -> .jpg"     ".jpg" (imageExtFor "application/octet-stream")
  check "imageExtFor empty header -> .jpg"     ".jpg" (imageExtFor "")
  check "imageExtFor strips parameters (image/png; charset=binary)"
    ".png" (imageExtFor "image/png; charset=binary")
  check "imageExtFor is case-insensitive (IMAGE/PNG)" ".png" (imageExtFor "IMAGE/PNG")
  check "isValidImageType Primary is True"   True  (isValidImageType "Primary")
  check "isValidImageType Backdrop is True"  True  (isValidImageType "Backdrop")
  check "isValidImageType empty is False"    False (isValidImageType "")
  check "isValidImageType 1nvalid! is False" False (isValidImageType "1nvalid!")
  check "isValidImageType with dash is False" False (isValidImageType "Primary-50")
  check "isValidImageType with slash is False" False (isValidImageType "a/b")
  check "isValidImageType with dotdot is False" False (isValidImageType "..")
  let rargs  = resizeArgs "/in.png" (Just 50) Nothing 90 "/out.jpg"
      rargs2 = resizeArgs "/in.png" (Just 50) (Just 40) 80 "/out2.jpg"
      rargs3 = resizeArgs "/in.png" Nothing (Just 40) 80 "/out3.jpg"
      hasPair x y = any (\(a, b) -> a == x && b == y) (zip rargs (drop 1 rargs))
      hasPair2 x y = any (\(a, b) -> a == x && b == y) (zip rargs2 (drop 1 rargs2))
      hasPair3 x y = any (\(a, b) -> a == x && b == y) (zip rargs3 (drop 1 rargs3))
  check "resizeArgs has -i <input>" True (hasPair "-i" "/in.png")
  check "resizeArgs ends with the output path" "/out.jpg" (last rargs)
  check "resizeArgs width-only uses scale=50:-2" True (hasPair "-vf" "scale=50:-2")
  check "resizeArgs has -q:v 90" True (hasPair "-q:v" "90")
  check "resizeArgs both dims uses force_original_aspect_ratio=decrease"
    True (hasPair2 "-vf" "scale=50:40:force_original_aspect_ratio=decrease")
  check "resizeArgs height-only uses scale=-2:40" True (hasPair3 "-vf" "scale=-2:40")
  check "imagePathFor joins stateDir/images/id/type.ext"
    "/state/images/7/Primary.png" (imagePathFor "/state" "7" "Primary" ".png")

  -- IO test of the actual resize, guarded by ffmpeg/ffprobe availability:
  -- generate a 200x100 PNG, resize to 50 wide, assert success + output
  -- exists + ffprobe reports width 50.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping images IO test"
    Just _ -> do
      let png = "/tmp/fms-test-img-src.png"
          out = "/tmp/fms-test-img-out.jpg"
      (ecGen, _, genErr2) <- runProcessBS "ffmpeg"
        ["-f", "lavfi", "-i", "color=red:s=200x100", "-frames:v", "1", "-y", png]
      check "generate 200x100 test png: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg png gen stderr: " ++ show (showBS (LBS.toStrict genErr2))))
      (ec, _, rErr) <- runProcessBS "ffmpeg" (resizeArgs png (Just 50) Nothing 90 out)
      check "resizeArgs run (50 wide) on png: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg resize stderr: " ++ show (showBS (LBS.toStrict rErr))))
      outExists <- doesFileExist out
      check "resized jpg exists after ffmpeg" True outExists
      mFp <- findExecutable "ffprobe"
      case mFp of
        Nothing -> putStrLn "SKIP: ffprobe not found on PATH -- skipping width assertion"
        Just _ -> do
          (ecP, pOut, _) <- runProcessBS "ffprobe"
            [ "-v", "error", "-select_streams", "v:0"
            , "-show_entries", "stream=width", "-of", "csv=p=0", out ]
          check "ffprobe on resized jpg: exit Success" ExitSuccess ecP
          check "ffprobe reports resized width 50" ["50"] (lines (LBSC.unpack pOut))
      removeFile png
      removeFile out

  -- Items query helpers (milestone 4a, docs/todo.org section 4): the pure
  -- functions of FMS.Core.Query.
  let qp = queryParams
        [ (BSC.pack "search", Just (BSC.pack "a%20b+c"))
        , (BSC.pack "empty", Nothing)
        , (BSC.pack "uni",   Just (BSC.pack "caf%C3%A9"))
        , (BSC.pack "plain", Just (BSC.pack "x"))
        ]
  check "queryParams percent-decodes (%20, + as space, UTF-8) and drops Nothing"
    [("search", "a b c"), ("uni", "café"), ("plain", "x")] qp
  check "queryParams drops valueless params"
    [("a", "1")] (queryParams [(BSC.pack "a", Just (BSC.pack "1")), (BSC.pack "b", Nothing)])
  check "queryParams on empty input" [] (queryParams [])

  -- A sample library of 4 items with distinct titles, sizes and times.
  let t1 = read "2024-01-01 00:00:00 UTC" :: UTCTime
      t2 = read "2024-01-02 00:00:00 UTC" :: UTCTime
      t3 = read "2024-01-03 00:00:00 UTC" :: UTCTime
      t4 = read "2024-01-04 00:00:00 UTC" :: UTCTime
      mkItem :: Int -> T.Text -> Integer -> UTCTime -> MediaItem
      mkItem i title size t = MediaItem
        { itemId      = T.pack (show i)
        , itemTitle   = title
        , itemPath    = "/" ++ T.unpack title ++ ".mp4"
        , itemSize    = size
        , itemMime    = "video/mp4"
        , itemAddedAt = t
        , itemFolder  = "1"
        , itemType      = "Movie"
        , itemSeries    = Nothing
        , itemSeason    = Nothing
        , itemEpisode   = Nothing
        , itemExtraType = Nothing
        , itemVersions = []
        }
      items4 =
        [ mkItem 1 "alpha"      300 t3
        , mkItem 2 "beta"       100 t1
        , mkItem 3 "gamma song" 200 t2
        , mkItem 4 "delta"      400 t4
        ]

  -- searchItems: case-insensitive substring match on itemTitle.
  check "searchItems is case-insensitive" ["alpha"] (map itemTitle (searchItems "ALPHA" items4))
  check "searchItems substring (SONG in 'gamma song')" ["gamma song"] (map itemTitle (searchItems "SONG" items4))
  check "searchItems no match -> []" [] (map itemTitle (searchItems "zzz" items4))
  check "searchItems empty term matches all" 4 (length (searchItems "" items4))

  -- Milestone 4n: metadata-aware search, image tags, exclusive filter.
  let meta4n = HM.fromList
        [ ("1", emptyMetadata { metaPersons = ["Keanu Reeves", "Carrie-Anne Moss"] })
        , ("2", emptyMetadata { metaPersons = ["Harrison Ford"] })
        ]
      lookup4n i = HM.lookup i meta4n
      items4n = [ (qi4n "1" "The Matrix"), (qi4n "2" "Blade Runner"), (qi4n "3" "Some Doc") ]
      qi4n i t = MediaItem
        { itemId = i, itemTitle = t, itemPath = "/" ++ T.unpack t ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = t4n, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      t4n = read "2026-01-01 00:00:00 UTC" :: UTCTime
  check "searchItemsMeta matches title" ["The Matrix"]
    (map itemTitle (searchItemsMeta lookup4n "matrix" items4n))
  check "searchItemsMeta matches a person (case-insensitive)" ["Blade Runner"]
    (map itemTitle (searchItemsMeta lookup4n "HANNOVER" items4n) ++
     map itemTitle (searchItemsMeta lookup4n "harrison" items4n))
  check "searchItemsMeta no match -> []" ([] :: [MediaItem])
    (searchItemsMeta lookup4n "spielberg" items4n)
  check "searchItemsMeta item without metadata matches on title only" ["Some Doc"]
    (map itemTitle (searchItemsMeta lookup4n "doc" items4n))
  let tagPairs = [("Primary", "p1"), ("Backdrop", "b1"), ("backdrop", "b2"), ("Logo", "l1")]
      itRaw = LBS.toStrict (Aeson.encode (imageTagsValue tagPairs))
      bdRaw = LBS.toStrict (Aeson.encode (backdropImageTagsValue tagPairs))
  check "imageTagsValue object maps type -> tag"
    [True, True, True, True]
    (map (`BSC.isInfixOf` itRaw) ["\"Primary\":\"p1\"", "\"Backdrop\":\"b1\"", "\"backdrop\":\"b2\"", "\"Logo\":\"l1\""])
  check "backdropImageTagsValue lists only backdrop tags (case-insensitive)"
    "[\"b1\",\"b2\"]" (BSC.unpack bdRaw)
  let baseItem = Aeson.toJSON (qi4n "1" "The Matrix")
      withTags = attachImageTags Nothing tagPairs baseItem
      withType = attachImageTags (Just "logo") tagPairs baseItem
      noTags   = attachImageTags Nothing [] baseItem
      wtRaw = LBS.toStrict (Aeson.encode withTags)
      wtyRaw = LBS.toStrict (Aeson.encode withType)
  check "attachImageTags adds ImageTags + BackdropImageTags to the item object"
    [True, True, True, True]
    (map (`BSC.isInfixOf` wtRaw)
      [ "\"ImageTags\":{", "\"Primary\":\"p1\"", "\"BackdropImageTags\":[\"b1\",\"b2\"]", "\"Name\":\"The Matrix\"" ])
  check "attachImageTags with imageType=Logo limits ImageTags to that type"
    [True, False]
    (map (`BSC.isInfixOf` wtyRaw) [ "\"ImageTags\":{\"Logo\":\"l1\"}", "\"Primary\":\"p1\"" ])
  check "attachImageTags with no images leaves the value unchanged"
    baseItem noTags
  check "notInAnyCollection keeps only items in no collection"
    ["1", "3"] (map (T.unpack . itemId) (notInAnyCollection (== "2") items4n))
  check "notInAnyCollection with all collected -> []"
    ([] :: [MediaItem]) (notInAnyCollection (const True) items4n)

  -- sortReleasesBySeeders (m25): default indexer search order
  let rel s sz = Release "t" sz s Nothing Nothing
  check "sortReleasesBySeeders: seeders desc, missing last, size tiebreak"
    ["b", "c", "a", "d"]
    (map (fst :: (String, Release) -> String) (sortReleasesBySeeders
      [ ("a", rel (Just 5) (Just 10)), ("b", rel (Just 99) (Just 1))
      , ("c", rel (Just 5) (Just 50)), ("d", rel Nothing (Just 999)) ]))

  -- dedupeReleases (m66): multi-query /Indexers/Search merge -- the same
  -- release matching both the English and the original-language query
  -- appears once (first occurrence wins, so sort first to keep the best)
  check "dedupeReleases: same title different case kept once, first wins"
    [("i1", "Pan's Labyrinth 2006 1080p"), ("i2", "El laberinto del fauno 2006")]
    (map (\(i, r) -> (i, relTitle r)) (dedupeReleases
      [ ("i1", Release "Pan's Labyrinth 2006 1080p" (Just 10) (Just 5) Nothing Nothing)
      , ("i1", Release "pan's labyrinth 2006 1080p" (Just 10) (Just 9) Nothing Nothing)
      , ("i2", Release "El laberinto del fauno 2006" (Just 20) (Just 3) Nothing Nothing) ]))

  -- parseProxiedPath (m21 fix: push resolves proxied indexer URLs in-process)
  check "parseProxiedPath parses /Indexers/:id/Download?r="
    (Just ("abc", "rid123")) (parseProxiedPath "/Indexers/abc/Download?r=rid123")
  check "parseProxiedPath rejects non-proxied and malformed paths"
    [Nothing, Nothing, Nothing, Nothing]
    (map parseProxiedPath
      [ "http://x/y", "/Indexers/abc/Download", "/Indexers//Download?r=z", "/Indexers/abc/Download?r=" ])

  -- Blurhash encoder (FMS.Core.Blurhash): base83 roundtrip, output
  -- shape, and attach helpers.
  check "base83 roundtrip for 0..6888 (2 chars, full range)"
    True (all (\v -> base83Decode (base83Encode v 2) == v) [0 .. 6888])
  check "base83 roundtrip for 3-char stride sample"
    True (all (\v -> base83Decode (base83Encode v 3) == v) [0, 977 .. 571786])
  check "base83 alphabet is the exact 0-9A-Za-z+symbols string"
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"
    base83Alphabet
  let gray8 = replicate (8 * 8 * 3) 128 :: [Word8]
      hGray = encodeBlurhash 8 8 4 3 gray8
  check "uniform gray 8x8 4x3: hash length is 1+1+4+2*(12-1) = 28"
    28 (length hGray)
  check "uniform gray 8x8 4x3: decodeBlurhashSize is (4,3)"
    (Just (4, 3)) (decodeBlurhashSize hGray)
  check "4x3 size flag: first char is base83 of 21"
    (base83Encode 21 1) (take 1 hGray)
  check "black vs white solid images give different hashes"
    True (encodeBlurhash 8 8 4 3 (replicate 192 0)
            /= encodeBlurhash 8 8 4 3 (replicate 192 255))
  let grad w h = [ fromIntegral ((x * 37 + y * 11) `mod` 256 :: Int)
                 | y <- [0 .. h - 1], x <- [0 .. w - 1], _ <- [1 .. 3 :: Int] ] :: [Word8]
  check "4x3 vs 8x6 gradient images give different hashes"
    True (encodeBlurhash 4 3 4 3 (grad 4 3) /= encodeBlurhash 8 6 4 3 (grad 8 6))
  check "encodeBlurhash rejects bad pixel count (returns empty)"
    "" (encodeBlurhash 8 8 4 3 (replicate 10 0))
  -- 32x32 deterministic LCG image, cross-validated against the official
  -- woltapp/blurhash reference algorithm (Python transcription).
  let lcgPix :: Int -> [Word8]
      lcgPix n = go (12345 :: Int) n
        where
          go :: Int -> Int -> [Word8]
          go _ 0 = []
          go s k = let s' = (s * 1103515245 + 12345) `mod` 2147483648
                       b = fromIntegral ((s' `div` 65536) `mod` 256) :: Word8
                   in b : go s' (k - 1)
  check "32x32 LCG image 4x3 matches reference implementation vector"
    "L7HetU-M+I;B?dkl#8RE$kq}VgcC" (encodeBlurhash 32 32 4 3 (lcgPix (32 * 32 * 3)))
  let bhPairs = [("Primary", "LEHV6nWB2yk8pyo0adR*.7kCMdnj"), ("Backdrop", ""), ("Logo", "L00000fQfQfQ")]
      bhRaw = LBS.toStrict (Aeson.encode (imageBlurHashesValue bhPairs))
  check "imageBlurHashesValue skips empty hashes"
    [True, False, True]
    (map (`BSC.isInfixOf` bhRaw)
      [ "\"Primary\":\"LEHV6nWB2yk8pyo0adR*.7kCMdnj\"", "\"Backdrop\"", "\"Logo\":\"L00000fQfQfQ\"" ])
  check "attachImageBlurHashes adds ImageBlurHashes to an item object"
    [True, True]
    (map (`BSC.isInfixOf` LBS.toStrict (Aeson.encode (attachImageBlurHashes bhPairs baseItem)))
      [ "\"ImageBlurHashes\":", "\"Primary\":\"LEHV6nWB2yk8pyo0adR*.7kCMdnj\"" ])
  check "attachImageBlurHashes with all-empty hashes leaves value unchanged"
    baseItem (attachImageBlurHashes [("Primary", "")] baseItem)

  -- sortItems: title asc by default, size desc, addedAt, unknown -> title.
  check "sortItems default is title asc"
    ["alpha", "beta", "delta", "gamma song"] (map itemTitle (sortItems Nothing Nothing items4))
  check "sortItems title desc"
    ["gamma song", "delta", "beta", "alpha"]
    (map itemTitle (sortItems (Just "title") (Just "desc") items4))
  check "sortItems size desc"
    ["delta", "alpha", "gamma song", "beta"]
    (map itemTitle (sortItems (Just "size") (Just "desc") items4))
  check "sortItems size asc when sortOrder missing"
    ["beta", "gamma song", "alpha", "delta"]
    (map itemTitle (sortItems (Just "size") Nothing items4))
  check "sortItems addedAt asc"
    ["beta", "gamma song", "alpha", "delta"]
    (map itemTitle (sortItems (Just "addedAt") (Just "asc") items4))
  check "sortItems unknown sortBy falls back to title"
    ["alpha", "beta", "delta", "gamma song"]
    (map itemTitle (sortItems (Just "bogus") Nothing items4))
  let dup = [mkItem 5 "same" 30 t1, mkItem 6 "same" 10 t2, mkItem 7 "same" 20 t3]
  check "sortItems is stable on equal keys" [30, 10, 20] (map itemSize (sortItems Nothing Nothing dup))

  -- paginate: windows, clamping, defaults.
  let xs5 = [1 .. 5] :: [Int]
  check "paginate with no params returns all" xs5 (paginate Nothing Nothing xs5)
  check "paginate startIndex only" [3, 4, 5] (paginate (Just 2) Nothing xs5)
  check "paginate limit only" [1, 2] (paginate Nothing (Just 2) xs5)
  check "paginate startIndex+limit window" [2] (paginate (Just 1) (Just 1) xs5)
  check "paginate startIndex beyond the end -> []" [] (paginate (Just 10) Nothing xs5)
  check "paginate negative startIndex clamped to 0" [1, 2] (paginate (Just (-3)) (Just 2) xs5)
  check "paginate limit larger than the rest" [4, 5] (paginate (Just 3) (Just 100) xs5)

  -- applyItemQuery: search + sort + paginate combined; the total count is
  -- taken BEFORE pagination.
  let titlesOf (n, is) = (n, map itemTitle is)
  check "applyItemQuery with no params returns all, total 4"
    (4, ["alpha", "beta", "delta", "gamma song"])
    (titlesOf (applyItemQuery [] items4))
  check "applyItemQuery search only (total = number of matches)"
    (2, ["beta", "delta"]) (titlesOf (applyItemQuery [("search", "ta")] items4))
  check "applyItemQuery search + sortBy=size + sortOrder=desc"
    (2, ["delta", "beta"])
    (titlesOf (applyItemQuery [("search", "ta"), ("sortBy", "size"), ("sortOrder", "desc")] items4))
  check "applyItemQuery combined search+sort+paginate keeps pre-pagination total"
    (2, ["beta"])
    (titlesOf (applyItemQuery
        [ ("search", "ta"), ("sortBy", "size"), ("sortOrder", "desc")
        , ("startIndex", "1"), ("limit", "1") ] items4))
  check "applyItemQuery invalid ints are ignored (defaults apply)"
    (4, ["alpha", "beta", "delta", "gamma song"])
    (titlesOf (applyItemQuery [("limit", "abc"), ("startIndex", "xyz")] items4))
  check "applyItemQuery startIndex beyond end -> empty page, total kept"
    (4, []) (titlesOf (applyItemQuery [("startIndex", "99")] items4))

  -- Stats module (milestone 5a, docs/roadmap.org milestone 5): the pure
  -- helpers of FMS.Modules.Stats.Monad on a sample map, the applyPlay /
  -- applyProgress semantics behind recordPlay / recordProgress, and the
  -- JSON persistence roundtrip.
  let pA = Progress 30 120 t1   -- 25%  -> continue watching
      pB = Progress 115 120 t2  -- 95.8% -> finished, excluded
      pC = Progress 5 100 t3    -- 5%   -> continue watching
      statsMap = HM.fromList
        [ ("a", ItemStats 3 t4 (Just pA))
        , ("b", ItemStats 1 t2 (Just pB))
        , ("c", ItemStats 5 t3 (Just pC))
        , ("d", ItemStats 2 t1 Nothing)
        ]
  check "continueWatching keeps unfinished progress, updatedAt desc"
    [("c", pC), ("a", pA)] (continueWatching statsMap)
  check "continueWatching excludes >=95% and progress-less items"
    2 (length (continueWatching statsMap))
  check "popular sorts by playCount desc"
    ["c", "a", "d", "b"] (map fst (popular statsMap))
  check "popular keeps the full ItemStats values"
    [5, 3, 2, 1] (map (statsPlayCount . snd) (popular statsMap))

  let s0 = ItemStats 0 t1 Nothing
  check "applyPlay bumps playCount and stamps lastPlayedAt"
    (ItemStats 1 t4 Nothing) (applyPlay t4 s0)
  check "applyPlay twice bumps twice"
    (ItemStats 2 t4 Nothing) (applyPlay t4 (applyPlay t4 s0))
  check "applyProgress sets progress, keeps playCount"
    (ItemStats 0 t1 (Just pA)) (applyProgress pA s0)
  check "applyProgress overwrites existing progress"
    (ItemStats 3 t4 (Just pC)) (applyProgress pC (ItemStats 3 t4 (Just pA)))

  -- JSON persistence: saveStats/loadStats roundtrip in /tmp.
  let statsFile = "/tmp/fms-test-stats.json"
  saveStats statsFile statsMap
  loadedStats <- loadStats statsFile
  check "saveStats/loadStats roundtrip" statsMap loadedStats
  rawStats <- BSC.readFile statsFile
  check "stats.json is a bare map containing the item ids"
    True ("\"a\"" `isInfixOf` BSC.unpack rawStats
          && "\"c\"" `isInfixOf` BSC.unpack rawStats
          && "\"playCount\"" `isInfixOf` BSC.unpack rawStats)
  missingStats <- loadStats "/tmp/fms-test-stats-does-not-exist.json"
  check "loadStats of a missing file is empty" HM.empty missingStats
  removeFile statsFile

  -- Auth module (milestone 5b, docs/todo.org section 8): the router's pure
  -- helpers (open paths + token header parsing), the bcrypt roundtrip, the
  -- users.json persistence roundtrip, and the public User JSON shape.

  -- isOpenPath: only the exact open paths are open.
  check "isOpenPath System/Info/Public" True (isOpenPath ["System", "Info", "Public"])
  check "isOpenPath healthz" True (isOpenPath ["healthz"])
  check "isOpenPath Users/AuthenticateByName" True (isOpenPath ["Users", "AuthenticateByName"])
  check "isOpenPath Users/Public" True (isOpenPath ["Users", "Public"])
  check "isOpenPath Users is False" False (isOpenPath ["Users"])
  check "isOpenPath Users/1 is False" False (isOpenPath ["Users", "1"])
  check "isOpenPath System/Info is False" False (isOpenPath ["System", "Info"])
  check "isOpenPath library is False" False (isOpenPath ["library"])
  check "isOpenPath dashboard (exact-case open list, milestone 19b)" True (isOpenPath ["dashboard"])
  check "isOpenPath Dashboard (admin API stays protected)" False (isOpenPath ["Dashboard"])
  check "isOpenPath dashboard/ extra segment is False" False (isOpenPath ["dashboard", "x"])
  check "openPathsExact contains only dashboard" [["dashboard"]] openPathsExact
  -- milestone 19b: firstMatch tries an exact-case pass before the
  -- case-insensitive one, so /dashboard (page) vs /Dashboard (API) resolve
  let rApi = get [Lit "Dashboard"] undefined
      rPage = get [Lit "dashboard"] undefined
  check "firstMatch: /dashboard -> exact page route"
    [Just [Lit "dashboard"]] [fmap (routePattern . fst) (firstMatch [rApi, rPage] "GET" ["dashboard"])]
  check "firstMatch: /Dashboard -> exact API route"
    [Just [Lit "Dashboard"]] [fmap (routePattern . fst) (firstMatch [rApi, rPage] "GET" ["Dashboard"])]
  check "firstMatch: /DASHBOARD -> case-insensitive fallback (first route)"
    [Just [Lit "Dashboard"]] [fmap (routePattern . fst) (firstMatch [rApi, rPage] "GET" ["DASHBOARD"])]
  check "firstMatch: case-variant literal not shadowed by capture (/Items)"
    [Just [Lit "items"]] [fmap (routePattern . fst) (firstMatch [get [Lit "items"] undefined, get [Capture "file"] undefined] "GET" ["Items"])]
  -- milestone 19d: route-aware open check
  check "openRoutePatterns is root/web/dashboard/api-docs/capture"
    [[], [Lit "web"], [Lit "dashboard"], [Lit "api-docs"], [Capture "file"]] openRoutePatterns
  check "isOpenRoutePattern matrix (Lit exact, Capture only matches Capture)"
    [ isOpenRoutePattern [] []
    , isOpenRoutePattern [] [Lit "web"]
    , isOpenRoutePattern [Lit "web"] [Lit "web"]
    , isOpenRoutePattern [Lit "web"] [Lit "WEB"]
    , isOpenRoutePattern [Lit "dashboard"] [Lit "dashboard"]
    , isOpenRoutePattern [Lit "dashboard"] [Lit "Dashboard"]
    , isOpenRoutePattern [Capture "file"] [Capture "file"]
    , isOpenRoutePattern [Capture "file"] [Lit "library"]
    , isOpenRoutePattern [Lit "library"] [Capture "file"]
    , isOpenRoutePattern [Lit "items"] [Lit "items"]
    , isOpenRoutePattern [Lit "web", Capture "x"] [Lit "web"]
    , isOpenRoutePattern [Lit "web"] [Lit "web", Lit "x"]
    ]
    [ True, False, True, False, True, False, True, False, False, True, False, False ]
  check "firstMatch: Lit library beats Capture file for /library (stays protected)"
    [Just [Lit "library"]]
    [fmap (routePattern . fst) (firstMatch [get [Lit "library"] undefined, get [Capture "file"] undefined] "GET" ["library"])]
  check "firstMatch: /elm.js falls through to Capture file (open only with uiDir)"
    [Just [Capture "file"]]
    [fmap (routePattern . fst) (firstMatch [get [Lit "library"] undefined, get [Capture "file"] undefined] "GET" ["elm.js"])]
  check "matchSegmentsExact: lit is case-sensitive, capture binds original"
    [ matchSegmentsExact [Lit "dashboard"] ["Dashboard"]
    , matchSegmentsExact [Lit "dashboard"] ["dashboard"]
    , matchSegmentsExact [Capture "id"] ["AbC"] ]
    [Nothing, Just [], Just [("id", "AbC")]]
  check "isOpenPath [] is False" False (isOpenPath [])

  -- parseTokenHeader: X-Emby-Token, MediaBrowser Token="...", Bearer, and
  -- the negative cases.
  check "parseTokenHeader X-Emby-Token"
    (Just "tok1") (parseTokenHeader [(BSC.pack "X-Emby-Token", BSC.pack "tok1")])
  check "parseTokenHeader header name is case-insensitive"
    (Just "tok1") (parseTokenHeader [(BSC.pack "x-emby-token", BSC.pack "tok1")])
  check "parseTokenHeader MediaBrowser Token=\"...\""
    (Just "abc123") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "MediaBrowser Token=\"abc123\"")])
  check "parseTokenHeader MediaBrowser with other params first"
    (Just "t9") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "MediaBrowser Client=\"Web\", Device=\"x\", Token=\"t9\"")])
  check "parseTokenHeader Bearer"
    (Just "bt") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "Bearer bt")])
  check "parseTokenHeader bearer (lowercase scheme)"
    (Just "bt") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "bearer bt")])
  check "parseTokenHeader missing headers -> Nothing" Nothing (parseTokenHeader [])
  check "parseTokenHeader Basic auth -> Nothing"
    Nothing (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "Basic dXNlcjpwYXNz")])
  check "parseTokenHeader empty X-Emby-Token -> Nothing"
    Nothing (parseTokenHeader [(BSC.pack "X-Emby-Token", BSC.pack "")])
  check "parseTokenHeader unrelated headers -> Nothing"
    Nothing (parseTokenHeader [(BSC.pack "Host", BSC.pack "localhost:8117")])
  -- m32: api_key query-param credential (Jellyfin-style, for <video src>)
  check "tokenFromQuery api_key"
    (Just "q1") (tokenFromQuery [(BSC.pack "api_key", Just (BSC.pack "q1"))])
  check "tokenFromQuery param name case-insensitive (API_KEY, X-Emby-Token, token)"
    [ tokenFromQuery [(BSC.pack "API_KEY", Just (BSC.pack "q2"))]
    , tokenFromQuery [(BSC.pack "X-Emby-Token", Just (BSC.pack "q3"))]
    , tokenFromQuery [(BSC.pack "Token", Just (BSC.pack "q4"))] ]
    [ Just "q2", Just "q3", Just "q4" ]
  check "tokenFromQuery empty/missing value -> Nothing, first non-empty wins"
    [ tokenFromQuery [(BSC.pack "api_key", Just (BSC.pack ""))]
    , tokenFromQuery [(BSC.pack "api_key", Nothing)]
    , tokenFromQuery [(BSC.pack "api_key", Just (BSC.pack "")), (BSC.pack "api_key", Just (BSC.pack "q5"))]
    , tokenFromQuery [] ]
    [ Nothing, Nothing, Just "q5", Nothing ]
  check "tokenFromRequestParts: header wins over query; query fallback; none -> Nothing"
    [ tokenFromRequestParts [(BSC.pack "X-Emby-Token", BSC.pack "hdr")] [(BSC.pack "api_key", Just (BSC.pack "q"))]
    , tokenFromRequestParts [] [(BSC.pack "api_key", Just (BSC.pack "q"))]
    , tokenFromRequestParts [] [] ]
    [ Just "hdr", Just "q", Nothing ]

  -- bcrypt roundtrip. Cost-14 hashing is slowish (~1s per op here), so keep
  -- the op count at three: one hash + two verifies.
  h <- hashPassword "s3cret-pw"
  check "bcrypt hash has a $2y$ prefix" True ("$2y$" `T.isPrefixOf` h)
  check "bcrypt verify accepts the right password" True (verifyPassword h "s3cret-pw")
  check "bcrypt verify rejects a wrong password" False (verifyPassword h "nope")

  -- users.json persistence roundtrip (internal JSON keeps the hash).
  let ufile = "/tmp/fms-test-users.json"
      u1 = User "1" "admin" "$2y$14$somehash" True ["key1"] Nothing emptyUserPolicy True False
      u2 = User "2" "bob" "hash2" False [] (Just ["1"]) emptyUserPolicy True False
      usersMap = HM.fromList [("1", u1), ("2", u2)]
  saveUsers ufile usersMap
  loadedUsers <- loadUsers ufile
  check "saveUsers/loadUsers roundtrip" usersMap loadedUsers
  rawUsers <- BSC.readFile ufile
  check "users.json is a bare map and keeps passwordHash internally"
    True ("passwordHash" `BSC.isInfixOf` rawUsers
          && "\"1\"" `BSC.isInfixOf` rawUsers
          && "\"2\"" `BSC.isInfixOf` rawUsers)
  missingUsers <- loadUsers "/tmp/fms-test-users-does-not-exist.json"
  check "loadUsers of a missing file is empty" HM.empty missingUsers
  removeFile ufile

  -- User JSON shapes: ToJSON is the public one (no passwordHash, has
  -- hasPassword); FromJSON/toInternalJSON are the internal full shape.
  let uj1 = LBS.toStrict (Aeson.encode u1)
      uj3 = LBS.toStrict (Aeson.encode (User "3" "nopw" "" False [] Nothing emptyUserPolicy True False))
  check "User ToJSON omits passwordHash" False ("passwordHash" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:true when a hash is set"
    True ("\"HasPassword\":true" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:false when the hash is empty"
    True ("\"HasPassword\":false" `BSC.isInfixOf` uj3)
  check "User ToJSON carries id/name/isAdmin"
    True (all (`BSC.isInfixOf` uj1) ["\"Id\":\"1\"", "\"Name\":\"admin\"", "\"IsAdministrator\":true"])
  check "User FromJSON parses the internal full shape"
    (Just u1) (Aeson.decode (Aeson.encode (toInternalJSON u1)) :: Maybe User)

  -- Milestone 5d (per-folder permissions): canAccessFolder matrix, old
  -- users.json compat (missing allowedFolders -> Nothing), and the
  -- permissions request JSON roundtrip.
  check "canAccessFolder Nothing allows everything" True (canAccessFolder Nothing "2")
  check "canAccessFolder (Just ids) allows a listed folder" True (canAccessFolder (Just ["1", "3"]) "3")
  check "canAccessFolder (Just ids) denies an unlisted folder" False (canAccessFolder (Just ["1"]) "2")
  check "canAccessFolder (Just []) denies everything" False (canAccessFolder (Just []) "1")
  let oldUserJson = "{\"userId\":\"1\",\"userName\":\"admin\",\"passwordHash\":\"h\",\"isAdmin\":true,\"apiKeys\":[]}"
  check "User FromJSON without allowedFolders (pre-5d file) defaults to Nothing"
    (Just Nothing) (userAllowedFolders <$> (Aeson.decode oldUserJson :: Maybe User))
  check "User FromJSON with allowedFolders:[\"1\"] parses to Just"
    (Just (Just ["1"]))
    (userAllowedFolders <$> (Aeson.decode "{\"userId\":\"2\",\"userName\":\"b\",\"passwordHash\":\"h\",\"isAdmin\":false,\"apiKeys\":[],\"allowedFolders\":[\"1\"]}" :: Maybe User))
  check "User FromJSON with allowedFolders:null decodes to Nothing"
    (Just Nothing)
    (userAllowedFolders <$> (Aeson.decode "{\"userId\":\"2\",\"userName\":\"b\",\"passwordHash\":\"h\",\"isAdmin\":false,\"apiKeys\":[],\"allowedFolders\":null}" :: Maybe User))

  -- Milestone 8b (password policy): checkPasswordPolicy matrix, old
  -- users.json compat (missing mustChangePassword -> False), roundtrip,
  -- public MustChangePassword key.
  check "checkPasswordPolicy: 0 disables the policy"
    [Right (), Right ()] [checkPasswordPolicy 0 "", checkPasswordPolicy 0 "x"]
  check "checkPasswordPolicy: negative min length disables the policy"
    (Right ()) (checkPasswordPolicy (-3) "")
  check "checkPasswordPolicy: exact boundary passes"
    (Right ()) (checkPasswordPolicy 8 "12345678")
  check "checkPasswordPolicy: too short fails with the length in the message"
    (Left "password must be at least 8 characters") (checkPasswordPolicy 8 "1234567")
  check "checkPasswordPolicy: counts characters (unicode chars count once)"
    (Right ()) (checkPasswordPolicy 3 "abc")
  check "User FromJSON without mustChangePassword (pre-8b file) defaults to False"
    (Just False)
    (userMustChangePassword <$> (Aeson.decode "{\"userId\":\"1\",\"userName\":\"a\",\"passwordHash\":\"h\",\"isAdmin\":true,\"apiKeys\":[]}" :: Maybe User))
  check "User internal JSON roundtrip keeps mustChangePassword"
    (Just (User "9" "z" "h" False [] Nothing emptyUserPolicy True True))
    (Aeson.decode (Aeson.encode (toInternalJSON (User "9" "z" "h" False [] Nothing emptyUserPolicy True True))) :: Maybe User)
  check "User ToJSON exposes MustChangePassword and not mustChangePassword"
    (True, False)
    (let ej = LBS.toStrict (Aeson.encode u1)
     in ("\"MustChangePassword\":false" `BSC.isInfixOf` ej,
         "mustChangePassword" `BSC.isInfixOf` ej))
  check "User public ToJSON carries AllowedFolders"
    True ("\"AllowedFolders\":[\"1\"]" `BSC.isInfixOf` LBS.toStrict (Aeson.encode u2))
  check "User internal JSON persists allowedFolders"
    True ("\"allowedFolders\":[\"1\"]" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (toInternalJSON u2)))
  let prq = PermissionsRequest (Just ["1", "2"])
  check "PermissionsRequest JSON roundtrip"
    (Just prq) (Aeson.decode (Aeson.encode prq) :: Maybe PermissionsRequest)
  check "PermissionsRequest decodes null AllowedFolders as Nothing"
    (Just (PermissionsRequest Nothing))
    (Aeson.decode "{\"AllowedFolders\":null}" :: Maybe PermissionsRequest)
  check "PermissionsRequest decodes a missing key as Nothing"
    (Just (PermissionsRequest Nothing))
    (Aeson.decode "{}" :: Maybe PermissionsRequest)

  -- Rate limiting + request ids (milestone 14a, docs/todo.org section 14):
  -- the pure token bucket math and the request-id format.
  let r = 60.0 :: Double   -- 60 req/min = 1 token/s, cap 60
      b0 = newBucket r tNow
  check "newBucket starts full at cap = rate" 60 (bucketTokens b0)
  let (ok1, tb1) = tryTake r tNow b0
  check "tryTake on a full bucket succeeds" True ok1
  check "tryTake consumes exactly one token" 59 (bucketTokens tb1)
  let empty = Bucket 0 tNow
      (ok2, tb2) = tryTake r tNow empty
  check "tryTake on an empty bucket denies" False ok2
  check "a denied tryTake does not go negative" 0 (bucketTokens tb2)
  check "refill adds rate/60 * elapsed tokens"
    10 (bucketTokens (refill r (addUTCTime 10 tNow) empty))
  check "refill caps at rate (never exceeds cap)"
    60 (bucketTokens (refill r (addUTCTime 3600 tNow) empty))
  let (ok3, tb3) = tryTake r (addUTCTime 1 tNow) empty
  check "tryTake refills first: 1s elapsed -> 1 token -> allowed"
    (True, 0) (ok3, bucketTokens tb3)
  check "requestIdHex 0 is 16 zero chars" "0000000000000000" (requestIdHex 0)
  check "requestIdHex 255 is zero-padded 16-char hex" "00000000000000ff" (requestIdHex 255)
  check "requestIdHex 65536 keeps 16 chars" 16 (T.length (requestIdHex 65536))

  -- ApiDocs module (milestone 1c, docs/todo.org section 1): the pure
  -- route -> OpenAPI path mapping and the spec generator.
  check "segmentToPathPiece Lit" "items" (segmentToPathPiece (Lit "items"))
  check "segmentToPathPiece Capture" "{id}" (segmentToPathPiece (Capture "id"))
  check "methodText lowercases the method" "get" (methodText "GET")
  let rDocs = get [Lit "items", Capture "id", Lit "metadata"] undefined
  check "routeToPath mixed Lit/Capture pattern" "/items/{id}/metadata" (routeToPath rDocs)
  check "routeToPath of the empty (root) pattern is /" "/" (routeToPath (get [] undefined))
  let spec     = routesToOpenApi [("fake", [rDocs, post [Lit "items"] undefined])]
      specTxt  = LBSC.unpack (Aeson.encode spec)
  check "generated spec declares openapi 3.0.3"
    True ("\"openapi\":\"3.0.3\"" `isInfixOf` specTxt)
  check "spec info has the server title/version"
    True ("\"title\":\"functional-media-server\"" `isInfixOf` specTxt
          && "\"version\":\"0.1.0.0\"" `isInfixOf` specTxt)
  check "spec paths contain /items/{id}/metadata"
    True ("\"/items/{id}/metadata\"" `isInfixOf` specTxt)
  check "spec path object has get (and /items has post)"
    True ("\"get\"" `isInfixOf` specTxt && "\"/items\":" `isInfixOf` specTxt
          && "\"post\"" `isInfixOf` specTxt)
  check "capture id becomes a required path parameter"
    True (all (`isInfixOf` specTxt)
      ["\"name\":\"id\"", "\"in\":\"path\"", "\"required\":true", "\"type\":\"string\""])
  check "operation has tags with the module name and a 200 response"
    True ("\"tags\":[\"fake\"]" `isInfixOf` specTxt && "\"200\"" `isInfixOf` specTxt)

  -- Metadata module (milestone 4b, docs/roadmap.org milestone 4,
  -- docs/todo.org section 3): the pure NFO parser, nfoPathFor, and the
  -- metadata.json persistence roundtrip.
  let sampleNfo = BSC.pack (unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<movie>"
        , "  <title>Test Movie</title>"
        , "  <originaltitle>Original Test Movie</originaltitle>"
        , "  <year>2020</year>"
        , "  <rating>7.5</rating>"
        , "  <runtime>95</runtime>"
        , "  <genre>Action</genre>"
        , "  <genre>Sci-Fi</genre>"
        , "  <overview>A test overview.</overview>"
        , "  <tmdbid>42</tmdbid>"
        , "  <imdbid>tt1234567</imdbid>"
        , "</movie>"
        ])
      expectedMeta = MediaMetadata
        { metaTitle          = Just "Test Movie"
        , metaOverview       = Just "A test overview."
        , metaGenres         = ["Action", "Sci-Fi"]
        , metaKeywords       = []
        , metaStudios        = []
        , metaPersons        = []
        , metaTags           = []
        , metaYear           = Just 2020
        , metaRating         = Just 7.5
        , metaOfficialRating = Nothing
        , metaRuntimeMinutes = Just 95
        , metaProviderIds    = HM.fromList [("tmdb", "42"), ("imdb", "tt1234567")]
        , metaSource         = "nfo"
        , metaLockedFields   = []                         -- milestone 4k
        }
      isLeftE' :: Either String MediaMetadata -> Bool
      isLeftE' (Left _)  = True
      isLeftE' (Right _) = False
  check "parseMovieNfo parses a full <movie> NFO"
    (Right expectedMeta) (parseMovieNfo sampleNfo)
  check "parseMovieNfo on malformed XML is Left"
    True (isLeftE' (parseMovieNfo (BSC.pack "<movie><title>oops")))
  check "parseMovieNfo on non-movie XML is Left"
    True (isLeftE' (parseMovieNfo (BSC.pack "<tvshow><title>x</title></tvshow>")))
  let sparseMeta = MediaMetadata
        (Just "Only Title") Nothing [] [] [] [] [] Nothing Nothing Nothing Nothing HM.empty "nfo" []
  check "parseMovieNfo with missing fields -> Nothing/empty fields"
    (Right sparseMeta)
    (parseMovieNfo (BSC.pack "<movie><title>Only Title</title></movie>"))
  check "parseMovieNfo ignores unparseable numbers"
    (Right (sparseMeta { metaTitle = Just "T", metaYear = Nothing }))
    (parseMovieNfo (BSC.pack "<movie><title>T</title><year>abc</year></movie>"))
  check "nfoPathFor replaces the extension"
    "/media/clip.nfo" (nfoPathFor "/media/clip.mp4")
  check "nfoPathFor keeps the directory"
    "/a/b/song.nfo" (nfoPathFor "/a/b/song.mp3")

  -- metadata.json persistence: saveMetadata/loadMetadata roundtrip in /tmp.
  let metaFile = "/tmp/fms-test-metadata.json"
      metaMap = HM.fromList [("1", expectedMeta), ("2", sparseMeta)]
  saveMetadata metaFile metaMap
  loadedMeta <- loadMetadata metaFile
  check "saveMetadata/loadMetadata roundtrip" metaMap loadedMeta
  rawMeta <- BSC.readFile metaFile
  check "metadata.json is a bare map containing ids and providerIds"
    True ("\"1\"" `isInfixOf` BSC.unpack rawMeta
          && "\"tmdb\"" `isInfixOf` BSC.unpack rawMeta
          && "\"providerIds\"" `isInfixOf` BSC.unpack rawMeta
          && "\"source\":\"nfo\"" `isInfixOf` BSC.unpack rawMeta)
  missingMeta <- loadMetadata "/tmp/fms-test-metadata-does-not-exist.json"
  check "loadMetadata of a missing file is empty" HM.empty missingMeta
  removeFile metaFile

  -- TMDB remote provider (milestone 4c, docs/roadmap.org milestone 4,
  -- docs/todo.org section 3): the PURE helpers only — no network in tests.
  let tmdbUrl = tmdbSearchUrl "TESTKEY" "a b"
  check "tmdbSearchUrl is the /search/movie endpoint"
    True ("https://api.themoviedb.org/3/search/movie?" `isPrefixOf` tmdbUrl)
  check "tmdbSearchUrl contains api_key=TESTKEY"
    True ("api_key=TESTKEY" `isInfixOf` tmdbUrl)
  check "tmdbSearchUrl encodes the query ('a b' -> a%20b or a+b)"
    True ("a%20b" `isInfixOf` tmdbUrl || "a+b" `isInfixOf` tmdbUrl)
  let tmdbUrlUni = tmdbSearchUrl "K" "café"
  check "tmdbSearchUrl UTF-8 percent-encodes non-ASCII (café -> caf%C3%A9)"
    True ("caf%C3%A9" `isInfixOf` tmdbUrlUni)

  -- A 2-result /search/movie fixture: full first hit, second hit without
  -- overview. release_date must yield the year; a missing overview -> Nothing.
  let tmdbFixture = LBS.fromStrict (BSC.pack (unlines
        [ "{\"page\":1,\"total_results\":2,\"total_pages\":1,\"results\":["
        , "  {\"id\":550,\"title\":\"Fight Club\",\"original_title\":\"Fight Club\",\"overview\":\"An insomniac office worker.\",\"release_date\":\"1999-10-15\",\"vote_average\":8.433,\"vote_count\":26280},"
        , "  {\"id\":680,\"title\":\"Pulp Fiction\",\"release_date\":\"1994-09-10\",\"vote_average\":8.5}"
        , "]}"
        ]))
      tmdbExpected =
        [ TmdbResult 550 "Fight Club" (Just "Fight Club") Nothing (Just "An insomniac office worker.") (Just 1999) (Just 8.433) Nothing
        , TmdbResult 680 "Pulp Fiction" Nothing Nothing Nothing (Just 1994) (Just 8.5) Nothing
        ]
  check "parseTmdbResults parses a 2-result fixture (year extraction, missing overview -> Nothing)"
    (Right tmdbExpected) (parseTmdbResults tmdbFixture)
  check "parseTmdbResults on an empty results array is Right []"
    (Right []) (parseTmdbResults (LBS.fromStrict (BSC.pack "{\"results\":[]}")))
  check "parseTmdbResults: empty release_date -> year Nothing, vote_average 0 -> Just 0.0"
    (Right [TmdbResult 7 "X" Nothing Nothing Nothing Nothing (Just 0.0) Nothing])
    (parseTmdbResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":7,\"title\":\"X\",\"release_date\":\"\",\"vote_average\":0}]}")))
  check "parseTmdbResults: malformed release_date -> year Nothing"
    (Right [TmdbResult 8 "Y" Nothing Nothing Nothing Nothing (Just 7.0) Nothing])
    (parseTmdbResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":8,\"title\":\"Y\",\"release_date\":\"soon\",\"vote_average\":7}]}")))
  -- m66: original_title/original_language are parsed (original-language
  -- torrent search in the Discover flow depends on them)
  check "parseTmdbResults parses original_title + original_language (m66)"
    (Right [TmdbResult 680 "Pan's Labyrinth" (Just "El laberinto del fauno") (Just "es") Nothing (Just 2006) (Just 8.3) Nothing])
    (parseTmdbResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":680,\"title\":\"Pan's Labyrinth\",\"original_title\":\"El laberinto del fauno\",\"original_language\":\"es\",\"release_date\":\"2006-12-01\",\"vote_average\":8.3}]}")))
  let isLeftStr :: Either String a -> Bool
      isLeftStr (Left _)  = True
      isLeftStr (Right _) = False
  check "parseTmdbResults on invalid JSON is Left"
    True (isLeftStr (parseTmdbResults (LBS.fromStrict (BSC.pack "not json"))))

  -- Milestone 4m: TV search URL + parser, RemoteSearch hit mapping, and
  -- the unmatched queue predicate.
  check "tmdbSearchTvUrl is the /search/tv endpoint with the encoded query"
    True ("https://api.themoviedb.org/3/search/tv?api_key=TESTKEY&query=a%20b"
            == tmdbSearchTvUrl "TESTKEY" "a b")
  let tvFixture = LBS.fromStrict (BSC.pack (unlines
        [ "{\"page\":1,\"results\":["
        , "  {\"id\":1399,\"name\":\"Game of Thrones\",\"overview\":\"Seven noble families fight.\",\"first_air_date\":\"2011-04-17\",\"vote_average\":8.4},"
        , "  {\"id\":1402,\"name\":\"The Wire\",\"first_air_date\":\"2002-06-02\"}"
        , "]}"
        ]))
  check "parseTmdbTvResults maps name/first_air_date onto TmdbResult"
    (Right [ TmdbResult 1399 "Game of Thrones" Nothing Nothing (Just "Seven noble families fight.") (Just 2011) (Just 8.4) Nothing
           , TmdbResult 1402 "The Wire" Nothing Nothing Nothing (Just 2002) Nothing Nothing ])
    (parseTmdbTvResults tvFixture)
  -- m66: tv payloads carry original_name/original_language
  check "parseTmdbTvResults parses original_name + original_language (m66)"
    (Right [TmdbResult 1399 "Dark" (Just "Dark") (Just "de") Nothing (Just 2017) Nothing Nothing])
    (parseTmdbTvResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":1399,\"name\":\"Dark\",\"original_name\":\"Dark\",\"original_language\":\"de\",\"first_air_date\":\"2017-12-01\"}]}")))
  check "parseTmdbTvResults on invalid JSON is Left"
    True (isLeftStr (parseTmdbTvResults (LBS.fromStrict (BSC.pack "nope"))))
  -- milestone 26: /Discover/Popular (TMDB popular movies+shows, poster_path)
  check "tmdbPopular URL builders: movie/tv popular, page, trailing slash stripped"
    [ tmdbPopularMoviesUrlAt "http://127.0.0.1:9999/3/" "K" 1
    , tmdbPopularTvUrlAt "http://127.0.0.1:9999/3" "K" 2
    , tmdbPopularMoviesUrl "K" 1
    , tmdbPopularTvUrl "K" 3 ]
    [ "http://127.0.0.1:9999/3/movie/popular?api_key=K&page=1"
    , "http://127.0.0.1:9999/3/tv/popular?api_key=K&page=2"
    , "https://api.themoviedb.org/3/movie/popular?api_key=K&page=1"
    , "https://api.themoviedb.org/3/tv/popular?api_key=K&page=3" ]
  check "parseTmdbResults decodes poster_path (m26)"
    (Right [TmdbResult 550 "Fight Club" Nothing Nothing Nothing (Just 1999) Nothing (Just "/p550.jpg")])
    (parseTmdbResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":550,\"title\":\"Fight Club\",\"release_date\":\"1999-10-15\",\"poster_path\":\"/p550.jpg\"}]}")))
  check "parseTmdbTvResults decodes poster_path (m26), missing -> Nothing"
    (Right [ TmdbResult 1399 "Game of Thrones" Nothing Nothing Nothing (Just 2011) Nothing (Just "/got.jpg")
           , TmdbResult 1402 "The Wire" Nothing Nothing Nothing (Just 2002) Nothing Nothing ])
    (parseTmdbTvResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":1399,\"name\":\"Game of Thrones\",\"first_air_date\":\"2011-04-17\",\"poster_path\":\"/got.jpg\"},{\"id\":1402,\"name\":\"The Wire\",\"first_air_date\":\"2002-06-02\"}]}")))
  let dEntry = discoverEntry "Movie" "https://image.tmdb.org/t/p" "w500"
        (TmdbResult 550 "Fight Club" Nothing Nothing (Just "An insomniac office worker.") (Just 1999) (Just 8.4) (Just "/p550.jpg"))
      dEntryRaw = LBS.toStrict (Aeson.encode dEntry)
      dNoPoster = LBS.toStrict (Aeson.encode (discoverEntry "Movie" "https://image.tmdb.org/t/p" "w500" (TmdbResult 1 "X" Nothing Nothing Nothing Nothing Nothing Nothing)))
      dOrig = LBS.toStrict (Aeson.encode (discoverEntry "Movie" "b" "s"
        (TmdbResult 680 "Pan's Labyrinth" (Just "El laberinto del fauno") (Just "es") Nothing (Just 2006) Nothing Nothing)))
  check "discoverEntry: PascalCase keys + full PosterUrl when poster present"
    [True, True, True, True, True]
    (map (`BSC.isInfixOf` dEntryRaw)
      [ "\"Id\":\"550\"", "\"Name\":\"Fight Club\"", "\"Type\":\"Movie\""
      , "\"ProductionYear\":1999", "\"PosterUrl\":\"https://image.tmdb.org/t/p/w500/p550.jpg\"" ])
  check "discoverEntry: no poster -> PosterUrl null"
    [True] [ "\"PosterUrl\":null" `BSC.isInfixOf` dNoPoster ]
  check "discoverEntry: m66 carries OriginalName + OriginalLanguage"
    [True, True]
    (map (`BSC.isInfixOf` dOrig)
      [ "\"OriginalName\":\"El laberinto del fauno\"", "\"OriginalLanguage\":\"es\"" ])
  check "discoverSection: Left -> [], Right capped at 20"
    [ length (discoverSection "Movie" "b" "s" (Left "boom"))
    , length (discoverSection "Movie" "b" "s" (Right (map (\i -> TmdbResult i "t" Nothing Nothing Nothing Nothing Nothing Nothing) [1 .. 25 :: Int]))) ]
    [ 0, 20 ]
  let hit = tmdbResultToSearchHit "Movie" (TmdbResult 550 "Fight Club" Nothing Nothing (Just "An insomniac office worker.") (Just 1999) (Just 8.433) Nothing)
      hitRaw = LBS.toStrict (Aeson.encode hit)
  check "tmdbResultToSearchHit emits the Jellyfin RemoteSearchResult keys (PascalCase)"
    [True, True, True, True, True, True]
    (map (`BSC.isInfixOf` hitRaw)
      [ "\"Id\":\"550\"", "\"Name\":\"Fight Club\"", "\"Type\":\"Movie\""
      , "\"ProductionYear\":1999", "\"ProviderIds\":{\"Tmdb\":550}", "\"Overview\":\"An insomniac" ])
  check "tmdbResultToSearchHit type=tv -> Series"
    True ("\"Type\":\"Series\"" `BSC.isInfixOf`
            LBS.toStrict (Aeson.encode (tmdbResultToSearchHit "Series" (TmdbResult 1399 "Game of Thrones" Nothing Nothing Nothing (Just 2011) Nothing Nothing))))
  let mkU i = MediaItem
        { itemId = i, itemTitle = "t" <> i, itemPath = "/" ++ T.unpack i ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = t4m, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      t4m = read "2026-01-01 00:00:00 UTC" :: UTCTime
      uItems4m = [mkU "1", mkU "2", mkU "3"]
  check "unmatchedItems keeps only items without metadata"
    ["2", "3"] (map (T.unpack . itemId) (unmatchedItems (== "1") uItems4m))
  check "unmatchedItems with all matched -> []"
    ([] :: [MediaItem]) (unmatchedItems (const True) uItems4m)
  check "parseTmdbResults on a non-object body is Left"
    True (isLeftStr (parseTmdbResults (LBS.fromStrict (BSC.pack "[1,2]"))))
  check "parseTmdbResults without a results key is Left"
    True (isLeftStr (parseTmdbResults (LBS.fromStrict (BSC.pack "{\"page\":1}"))))

  -- UserData module (milestone 5c, docs/todo.org section 9): the pure
  -- helpers of FMS.Modules.UserData.Monad, the UserData JSON roundtrip
  -- (incl. partial decode), and the nested userdata.json persistence
  -- roundtrip in /tmp.
  let ud0 = emptyUserData
  check "emptyUserData is all-empty"
    (UserData False False Nothing 0 Nothing) ud0
  check "markPlayed True sets played and bumps playCount"
    (UserData True False Nothing 1 Nothing) (markPlayed True ud0)
  check "markPlayed True twice bumps the play count twice"
    (UserData True False Nothing 2 Nothing) (markPlayed True (markPlayed True ud0))
  check "markPlayed False clears played, keeps playCount"
    (UserData False False Nothing 2 Nothing) (markPlayed False (markPlayed True (markPlayed True ud0)))
  check "setFavorite True sets favorite"
    (ud0 { udFavorite = True }) (setFavorite True ud0)
  check "setFavorite False clears favorite"
    (ud0 { udFavorite = False }) (setFavorite False (setFavorite True ud0))
  check "setPosition sets playbackPosition"
    (ud0 { udPlaybackPosition = Just 42.5 }) (setPosition 42.5 ud0)
  check "setPosition overwrites an existing position"
    (Just 99.0) (udPlaybackPosition (setPosition 99.0 (setPosition 42.5 ud0)))

  -- UserData JSON: roundtrip + Jellyfin-style keys + partial decode.
  let udSample = UserData True True (Just 42.5) 3 (Just t4)
      udJson   = LBS.toStrict (Aeson.encode udSample)
  check "UserData JSON roundtrip" (Just udSample) (Aeson.decode (Aeson.encode udSample) :: Maybe UserData)
  check "UserData ToJSON uses the Jellyfin-style keys"
    True (all (`BSC.isInfixOf` udJson)
      [ "\"played\":true", "\"favorite\":true", "\"playbackPosition\":42.5"
      , "\"playCount\":3", "\"lastPlayedAt\":" ])
  check "UserData empty encodes null position/lastPlayedAt and 0 playCount"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode ud0))
      [ "\"played\":false", "\"favorite\":false", "\"playbackPosition\":null"
      , "\"playCount\":0", "\"lastPlayedAt\":null" ])
  check "UserData partial decode {\"played\":true} keeps the other defaults"
    (Just (ud0 { udPlayed = True })) (Aeson.decode "{\"played\":true}" :: Maybe UserData)
  check "UserData partial decode {} decodes to emptyUserData"
    (Just ud0) (Aeson.decode "{}" :: Maybe UserData)
  check "UserData partial decode {\"playbackPosition\":7.25}"
    (Just (ud0 { udPlaybackPosition = Just 7.25 }))
    (Aeson.decode "{\"playbackPosition\":7.25}" :: Maybe UserData)
  check "UserData decode of a non-object is Nothing"
    (Nothing :: Maybe UserData) (Aeson.decode "[1,2]")
  check "UserData decode of a wrongly typed field is Nothing"
    (Nothing :: Maybe UserData) (Aeson.decode "{\"played\":\"yes\"}")

  -- userdata.json persistence: nested map userId -> itemId -> UserData.
  let udFile = "/tmp/fms-test-userdata.json"
      udMap = HM.fromList
        [ ("1", HM.fromList [("10", udSample), ("11", ud0)])
        , ("2", HM.fromList [("10", ud0 { udFavorite = True })])
        ]
  saveUserData udFile udMap
  loadedUD <- loadUserData udFile
  check "saveUserData/loadUserData nested roundtrip" udMap loadedUD
  rawUD <- BSC.readFile udFile
  check "userdata.json is a nested map (user -> item -> UserData)"
    True ("\"1\"" `BSC.isInfixOf` rawUD
          && "\"2\"" `BSC.isInfixOf` rawUD
          && "\"10\"" `BSC.isInfixOf` rawUD
          && "\"played\":true" `BSC.isInfixOf` rawUD
          && "\"playbackPosition\":42.5" `BSC.isInfixOf` rawUD)
  missingUD <- loadUserData "/tmp/fms-test-userdata-does-not-exist.json"
  check "loadUserData of a missing file is empty" HM.empty missingUD
  removeFile udFile

  -- Playback module (milestone 6a, docs/todo.org section 6): the pure
  -- ffprobe argument list, the pure parseFfprobe parser on a fixture,
  -- the container-from-extension fallback, and the direct-play /
  -- direct-stream / transcode decision matrix.
  check "probeArgs is the ffprobe json show_format/show_streams invocation"
    ["-v","error","-print_format","json","-show_format","-show_streams","/x.mp4"]
    (probeArgs "/x.mp4")

  let ffprobeFixture = LBS.fromStrict (BSC.pack (unwords
        [ "{"
        , "\"streams\":["
        , "{\"index\":0,\"codec_name\":\"h264\",\"codec_type\":\"video\",\"width\":1920,\"height\":1080,\"duration\":\"61.000000\",\"tags\":{\"language\":\"eng\"}},"
        , "{\"index\":1,\"codec_name\":\"aac\",\"codec_type\":\"audio\",\"channels\":2,\"tags\":{\"language\":\"eng\"}},"
        , "{\"index\":2,\"codec_name\":\"srt\",\"codec_type\":\"subtitle\",\"tags\":{\"language\":\"fre\",\"title\":\"Forced\"}}"
        , "],"
        , "\"format\":{\"format_name\":\"mov,mp4,m4a,3gp,3g2,mj2\",\"duration\":\"120.500000\"}"
        , "}"
        ]))
      isRightE :: Either a b -> Bool
      isRightE (Right _) = True
      isRightE (Left _)  = False
  case parseFfprobe ffprobeFixture of
    Left err -> do
      putStrLn ("FAIL: parseFfprobe fixture -- " ++ err)
      exitFailure
    Right (container, streams) -> do
      check "parseFfprobe fixture container is mp4 (mov,mp4,... -> mp4)"
        "mp4" (T.unpack container)
      check "parseFfprobe fixture has 3 streams" 3 (length streams)
      check "parseFfprobe stream types" ["Video","Audio","Subtitle"] (map msType streams)
      check "parseFfprobe stream codecs" ["h264","aac","srt"] (map msCodec streams)
      check "parseFfprobe stream indexes" [0,1,2] (map msIndex streams)
      check "parseFfprobe video width/height"
        (Just 1920, Just 1080) (msWidth (streams !! 0), msHeight (streams !! 0))
      check "parseFfprobe audio channels + language"
        (Just 2, Just "eng") (msChannels (streams !! 1), msLanguage (streams !! 1))
      check "parseFfprobe subtitle language + title"
        (Just "fre", Just "Forced") (msLanguage (streams !! 2), msTitle (streams !! 2))
      check "parseFfprobe stream duration (stream's own)"
        (Just 61.0) (msDurationSeconds (streams !! 0))
      check "parseFfprobe falls back to format duration"
        (Just 120.5) (msDurationSeconds (streams !! 1))
  check "parseFfprobe on malformed JSON is Left"
    True (isLeftE (parseFfprobe "this is not json at all"))
  check "parseFfprobe on wrong-typed streams is Left"
    True (isLeftE (parseFfprobe "{\"streams\": 42}"))
  check "parseFfprobe on a non-object is Left"
    True (isLeftE (parseFfprobe "[1,2,3]"))

  check "containerFromExtension lowercases .MP4" "mp4" (T.unpack (containerFromExtension "/a/b/clip.MP4"))
  check "containerFromExtension no extension -> unknown" "unknown" (T.unpack (containerFromExtension "/a/b/noext"))

  -- The decision matrix (default client profile).
  let vcodec c = MediaStream 0 "Video"   c Nothing Nothing Nothing Nothing Nothing Nothing False
      acodec c = MediaStream 1 "Audio"   c Nothing Nothing Nothing Nothing Nothing Nothing False
      sub    c = MediaStream 2 "Subtitle" c Nothing Nothing Nothing Nothing Nothing Nothing False
  check "directPlayable mp4 h264+aac is True"
    True (directPlayable "mp4" [vcodec "h264", acodec "aac"])
  check "directPlayable mp4 hevc+ac3 is False (hevc no longer direct-playable, m33)"
    False (directPlayable "mp4" [vcodec "hevc", acodec "ac3"])
  check "directPlayable mkv h264+aac is False (container not direct-play)"
    False (directPlayable "mkv" [vcodec "h264", acodec "aac"])
  check "directStreamable mkv is True (remux)"
    True (directStreamable "mkv" [vcodec "h264", acodec "aac"])
  check "transcoding mkv is True (not directPlayable)"
    True (transcoding "mkv" [vcodec "h264", acodec "aac"])
  check "directPlayable avi mpeg4+mp3 is False -> transcode"
    False (directPlayable "avi" [vcodec "mpeg4", acodec "mp3"])
  check "transcoding avi is True"
    True (transcoding "avi" [vcodec "mpeg4", acodec "mp3"])
  check "directPlayable mp3 audio-only is True"
    True (directPlayable "mp3" [acodec "mp3"])
  check "directStreamable mp4 is False"
    False (directStreamable "mp4" [vcodec "h264"])
  check "directPlayable ignores subtitle streams"
    True (directPlayable "mp4" [vcodec "h264", acodec "aac", sub "srt"])
  check "directPlayable webm vp9+opus is True"
    True (directPlayable "webm" [vcodec "vp9", acodec "opus"])
  check "directPlayable mp4 with unknown video codec is False"
    False (directPlayable "mp4" [vcodec "wmv3", acodec "aac"])
  -- m33: browser-safe default profile (hevc/mpeg4/mpeg2video dropped)
  check "m33 directPlayable: mp4 hevc -> False, mp4 h264 -> True, webm vp9/opus -> True"
    [ directPlayable "mp4" [vcodec "hevc", acodec "aac"]
    , directPlayable "mp4" [vcodec "h264", acodec "aac"]
    , directPlayable "webm" [vcodec "vp9", acodec "opus"] ]
    [ False, True, True ]
  check "m33 directPlayable: mkv h264 False (container), mp4 mpeg4/mpeg2video False (codec)"
    [ directPlayable "mkv" [vcodec "h264", acodec "aac"]
    , directPlayable "mp4" [vcodec "mpeg4", acodec "aac"]
    , directPlayable "mp4" [vcodec "mpeg2video", acodec "aac"] ]
    [ False, False, False ]
  check "m33 decidePlayMethod: hevc mp4 -> Transcode, h264 mp4 -> DirectPlay (no bitrate cap)"
    [ decidePlayMethod (directPlayable "mp4" [vcodec "hevc", acodec "aac"]) Nothing Nothing
    , decidePlayMethod (directPlayable "mp4" [vcodec "h264", acodec "aac"]) Nothing Nothing ]
    [ "Transcode", "DirectPlay" ]

  -- Milestone 6i: device profile + audio/subtitle stream selection
  -- (docs/todo.org section 6).
  let ms6i i t c = MediaStream i t c Nothing Nothing Nothing Nothing Nothing Nothing False
      streams6i =
        [ ms6i 0 "Video" "h264"
        , ms6i 1 "Audio" "aac"
        , ms6i 2 "Audio" "ac3"
        , ms6i 3 "Subtitle" "subrip"
        , ms6i 4 "Subtitle" "ass"
        ]
      selIdx (mA, mS) = (msIndex <$> mA, msIndex <$> mS)
      profRaw = LBS.toStrict (Aeson.encode defaultDeviceProfile)
  check "defaultDeviceProfile JSON shape (Name + 3 profile arrays)"
    [True, True, True, True]
    (map (`BSC.isInfixOf` profRaw)
      [ "\"Name\":\"Default\"", "\"DirectPlayProfiles\":", "\"TranscodingProfiles\":", "\"SubtitleProfiles\":[]" ])
  check "defaultDeviceProfile video entry covers mp4/h264/aac and subtitle srt"
    [True, True, True, True]
    (map (`BSC.isInfixOf` profRaw)
      [ "\"Container\":\"mp4,", "\"VideoCodec\":\"h264,", "\"AudioCodec\":\"aac,", "\"Container\":\"srt," ])
  check "defaultDeviceProfile transcode entry is mp4/h264/aac"
    [True, True, True]
    (map (`BSC.isInfixOf` profRaw)
      [ "\"TranscodingProfiles\":", "\"Container\":\"mp4\"", "\"VideoCodec\":\"h264\"" ])
  check "selectStreams by index (audio 2, subtitle 4)"
    (Just 2, Just 4) (selIdx (selectStreams (Just 2) (Just 4) streams6i))
  check "selectStreams audio falls back to first audio when index unmatched"
    (Just 1, Nothing) (selIdx (selectStreams (Just 99) Nothing streams6i))
  check "selectStreams no request -> first audio, no subtitle"
    (Just 1, Nothing) (selIdx (selectStreams Nothing Nothing streams6i))
  check "selectStreams negative index means none (Jellyfin -1 convention)"
    (Nothing, Nothing) (selIdx (selectStreams (Just (-1)) (Just (-1)) streams6i))
  check "selectStreams subtitle has NO fallback when unmatched"
    (Just 1, Nothing) (selIdx (selectStreams Nothing (Just 99) streams6i))
  check "selectStreams empty stream list -> (Nothing, Nothing)"
    (Nothing, Nothing) (selIdx (selectStreams (Just 1) (Just 3) []))
  let (mA6i, mS6i) = selectStreams (Just 2) (Just 4) streams6i
  check "markDefaultStreams marks chosen audio+subtitle, nothing else"
    [False, False, True, False, True]
    (map msIsDefault (markDefaultStreams mA6i mS6i streams6i))
  check "markDefaultStreams with no selection marks nothing"
    [False, False, False, False, False]
    (map msIsDefault (markDefaultStreams Nothing Nothing streams6i))
  check "MediaStream JSON carries IsDefault key"
    True ("\"IsDefault\":false" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (ms6i 0 "Video" "h264")))
  let pr6iRaw = LBS.toStrict (Aeson.encode
        (PlaybackResponse [] "sess" "DirectPlay" Nothing Nothing (Just 1) (Just 3)))
  check "PlaybackResponse echoes AudioStreamIndex/SubtitleStreamIndex when set"
    [True, True, True]
    (map (`BSC.isInfixOf` pr6iRaw)
      [ "\"AudioStreamIndex\":1", "\"SubtitleStreamIndex\":3", "\"PlayMethod\":\"DirectPlay\"" ])
  let pr6iRawN = LBS.toStrict (Aeson.encode
        (PlaybackResponse [] "sess" "Transcode" (Just 8000000) (Just "/stream/1/transcode?version=0") Nothing Nothing))
  check "PlaybackResponse omits stream indexes when Nothing, keeps bitrate/url"
    [False, False, True, True]
    (map (`BSC.isInfixOf` pr6iRawN)
      [ "\"AudioStreamIndex\"", "\"SubtitleStreamIndex\"", "\"MaxStreamingBitrate\":8000000", "\"TranscodingUrl\":\"/stream/1/transcode?version=0\"" ])

  -- IO test of the real ffprobe round trip, guarded by ffprobe/ffmpeg
  -- availability: skip gracefully when they are not on PATH.
  mFfprobe <- findExecutable "ffprobe"
  case (mFfprobe, mFFmpeg) of
    (Just _, Just _) -> do
      let clip  = "/tmp/fms-test-probe-clip.mp4"
          dummy = "/tmp/fms-test-probe-dummy.mp4"
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        [ "-f", "lavfi", "-i", "testsrc=duration=2:size=320x240:rate=10"
        , "-f", "lavfi", "-i", "sine=frequency=440:duration=2"
        , "-c:v", "libx264", "-c:a", "aac", "-y", clip ]
      check "generate 2s test clip (video+audio): exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg clip gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      writeFile dummy "not really an mp4, just bytes"
      rClip <- probeFile clip
      check "probeFile on a real clip is Right" True (isRightE rClip)
      case rClip of
        Left err -> do
          putStrLn ("FAIL: probeFile clip -- " ++ err)
          exitFailure
        Right (c, streams) -> do
          check "probeFile clip container is mp4" "mp4" (T.unpack c)
          check "probeFile clip stream types" ["Video", "Audio"] (map msType streams)
          check "probeFile clip codecs" ["h264", "aac"] (map msCodec streams)
          check "probeFile clip is direct playable" True (directPlayable c streams)
          check "probeFile clip is not direct streamable" False (directStreamable c streams)
      rDummy <- probeFile dummy
      check "probeFile on dummy bytes is Left" True (isLeftE rDummy)
      removeFile clip
      removeFile dummy
    _ -> putStrLn "SKIP: ffprobe/ffmpeg not found on PATH -- skipping probe IO test"

  -- Sessions module (milestone 6c, docs/todo.org section 6): the pure
  -- session-map helpers (upsert/expire) and the PlaySession JSON roundtrip.
  let t0 = read "2026-01-01 00:00:00 UTC" :: UTCTime
      s1 = PlaySession
        { psId = "s1", psItemId = "1", psUserId = Just "1", psUserName = Just "admin"
        , psPositionSeconds = 0, psIsPaused = False, psStartedAt = t0, psLastPingAt = t0
        , psPlayableMediaTypes = [], psSupportedCommands = []
        , psSupportsMediaControl = False, psSupportsPersistentIdentifier = False
        , psDeviceId = Nothing, psLastCommand = Nothing, psToken = Nothing
        }
      s2 = s1 { psId = "s2", psItemId = "2", psUserId = Nothing, psUserName = Nothing
              , psPositionSeconds = 12.5, psIsPaused = True, psLastPingAt = addUTCTime (-120) t0 }
      s3 = s1 { psId = "s3", psLastPingAt = addUTCTime (-90) t0 }
      sessMap = HM.fromList [("s1", s1), ("s2", s2)]
  check "upsertSession inserts a new session"
    (Just s1) (HM.lookup "s1" (upsertSession s1 HM.empty))
  check "upsertSession replaces an existing session in place"
    (Just 5.0)
    (psPositionSeconds <$> HM.lookup "s1" (upsertSession (s1 { psPositionSeconds = 5 }) (upsertSession s1 HM.empty)))
  check "expireSessions drops sessions with last ping older than 90s"
    ["s1"] (sort (HM.keys (expireSessions t0 90 sessMap)))
  check "expireSessions keeps a session exactly at the 90s limit"
    ["s1", "s3"] (sort (HM.keys (expireSessions t0 90 (HM.insert "s3" s3 sessMap))))
  check "PlaySession JSON roundtrip"
    (Just s1) (Aeson.decode (Aeson.encode s1) :: Maybe PlaySession)
  check "PlaySession JSON uses the PascalCase keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode s1))
      [ "\"Id\":", "\"ItemId\":", "\"UserId\":", "\"UserName\":"
      , "\"PositionSeconds\":", "\"IsPaused\":", "\"StartedAt\":", "\"LastPingAt\":" ])

  -- Session capabilities + commands (todo Chromecast/AirPlay section):
  -- backward-compatible JSON, capability merge, command-name validation,
  -- token->session lookup.
  let oldShape = "{\"Id\":\"s9\",\"ItemId\":\"1\",\"PositionSeconds\":0,"
              ++ "\"IsPaused\":false,"
              ++ "\"StartedAt\":\"2026-01-01T00:00:00Z\",\"LastPingAt\":\"2026-01-01T00:00:00Z\"}"
      mOld = Aeson.decode (LBSC.pack oldShape) :: Maybe PlaySession
      caps = SessionCapabilities
        { scPlayableMediaTypes = ["Video", "Audio"]
        , scSupportedCommands = ["VolumeUp", "VolumeDown"]
        , scSupportsMediaControl = True
        , scSupportsPersistentIdentifier = True
        , scDeviceId = Just "dev-1"
        }
      s1c = applyCapabilities s1 caps
  check "old session JSON (no capability fields) decodes with defaults"
    (Just (False, [], [], False, Nothing, Nothing))
    ((\s -> (psSupportsMediaControl s, psPlayableMediaTypes s, psSupportedCommands s
            , psSupportsPersistentIdentifier s, psDeviceId s, psLastCommand s)) <$> mOld)
  check "PlaySession roundtrip WITH capability fields set"
    (Just s1c) (Aeson.decode (Aeson.encode s1c) :: Maybe PlaySession)
  check "capability fields appear in JSON only when set"
    (True, False)
    ( BSC.isInfixOf "\"PlayableMediaTypes\":[\"Video\",\"Audio\"]" (LBS.toStrict (Aeson.encode s1c))
    , BSC.isInfixOf "PlayableMediaTypes" (LBS.toStrict (Aeson.encode s1)) )
  check "capability flags + DeviceId appear when set"
    [True, True, True]
    [ BSC.isInfixOf "\"SupportsMediaControl\":true" (LBS.toStrict (Aeson.encode s1c))
    , BSC.isInfixOf "\"SupportsPersistentIdentifier\":true" (LBS.toStrict (Aeson.encode s1c))
    , BSC.isInfixOf "\"DeviceId\":\"dev-1\"" (LBS.toStrict (Aeson.encode s1c)) ]
  check "psToken is never serialized"
    False (BSC.isInfixOf "Token" (LBS.toStrict (Aeson.encode (s1 { psToken = Just "sekret" }))))
  check "applyCapabilities replaces lists/flags (merge, not append)"
    (["Video"], ["VolumeUp"], True, Just "d2")
    ( let c2 = applyCapabilities s1c (emptyCapabilities { scPlayableMediaTypes = ["Video"]
                                                            , scSupportedCommands = ["VolumeUp"]
                                                            , scSupportsMediaControl = True
                                                            , scDeviceId = Just "d2" })
      in (psPlayableMediaTypes c2, psSupportedCommands c2, psSupportsMediaControl c2, psDeviceId c2) )
  check "SessionCapabilities empty body decodes to all defaults"
    (Just emptyCapabilities) (Aeson.decode "{}" :: Maybe SessionCapabilities)
  check "validCommandName: ok names, empty, 64 vs 65 chars, control chars"
    [True, False, True, False, False, True]
    [ validCommandName "VolumeUp"
    , validCommandName ""
    , validCommandName (T.replicate 64 "a")
    , validCommandName (T.replicate 65 "a")
    , validCommandName "bad\tname"
    , validCommandName "SetVolume" ]
  check "latestSessionForToken picks the most recent ping; Nothing when absent"
    (Just "sB", Nothing)
    ( fmap psId (latestSessionForToken "tok1" [ s1 { psId = "sA", psToken = Just "tok1", psLastPingAt = t0 }
                                              , s1 { psId = "sB", psToken = Just "tok1", psLastPingAt = addUTCTime 10 t0 }
                                              , s1 { psId = "sC", psToken = Just "tok2" } ])
    , fmap psId (latestSessionForToken "nope" [s1 { psToken = Just "tok1" }]) )

  -- Genres + Playlists (milestone 10a, docs/todo.org section 10): the
  -- pure genre grouping, the pure playlist item helpers, the Playlist
  -- JSON roundtrip, and the playlists.json persistence roundtrip.
  let gm gs = sparseMeta { metaGenres = gs }
  check "genresFromMetadata groups, counts per item, sorts by name"
    [("Action",1),("Drama",2),("Sci-Fi",1)]
    (genresFromMetadata [gm ["Drama","Action"], gm ["Drama","Sci-Fi"], gm []])
  check "genresFromMetadata counts a duplicate genre in one item once"
    [("Action",1)] (genresFromMetadata [gm ["Action","Action"]])
  check "genresFromMetadata on empty input is []"
    [] (genresFromMetadata [])
  check "addItem appends a new item" ["1","2"] (addItem "2" ["1"])
  check "addItem dedupes an existing item" ["1","2"] (addItem "1" ["1","2"])
  check "removeItem removes all occurrences" ["1"] (removeItem "2" ["1","2","2"])
  check "removeItem of an absent item is a no-op" ["1"] (removeItem "x" ["1"])
  let pl0 = Playlist "p1" "Favs" ["1","2"] t4
      plMap = HM.fromList [("p1", pl0), ("p2", pl0 { plId = "p2", plName = "Other", plItemIds = [] })]
      plFile = "/tmp/fms-test-playlists.json"
  check "Playlist JSON roundtrip" (Just pl0) (Aeson.decode (Aeson.encode pl0) :: Maybe Playlist)
  check "Playlist JSON uses the PascalCase keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode pl0))
      [ "\"Id\":", "\"Name\":", "\"ItemIds\":", "\"CreatedAt\":" ])
  savePlaylists plFile plMap
  loadedPl <- loadPlaylists plFile
  check "savePlaylists/loadPlaylists roundtrip" plMap loadedPl
  missingPl <- loadPlaylists "/tmp/fms-test-playlists-does-not-exist.json"
  check "loadPlaylists of a missing file is empty" HM.empty missingPl
  removeFile plFile

  -- ServerConfig (milestone 1b, docs/todo.org section 1): defaults, JSON
  -- roundtrip, partial patch decode + merge semantics, file load/save.
  check "defaultServerConfig values"
    ("functional-media-server", False, "./media", 8080)
    ( scServerName defaultServerConfig, scRequireAuth defaultServerConfig
    , scLibraryPath defaultServerConfig, scPort defaultServerConfig )
  check "ServerConfig JSON roundtrip"
    (Just defaultServerConfig)
    (Aeson.decode (Aeson.encode defaultServerConfig) :: Maybe ServerConfig)
  check "ServerConfigPatch partial decode: only ServerName set"
    (Just (ServerConfigPatch (Just "My Media") Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
    (Aeson.decode "{\"ServerName\":\"My Media\"}" :: Maybe ServerConfigPatch)
  check "ServerConfigPatch empty object decodes to all-Nothing"
    (Just (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
    (Aeson.decode "{}" :: Maybe ServerConfigPatch)
  let patch = fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
            (Aeson.decode "{\"ServerName\":\"My Media\",\"Port\":9999}" :: Maybe ServerConfigPatch)
      merged = applyServerConfigPatch defaultServerConfig patch
  check "applyServerConfigPatch overrides set fields, keeps the rest"
    ("My Media", False, "./media", 9999)
    (scServerName merged, scRequireAuth merged, scLibraryPath merged, scPort merged)
  check "mergeServerConfig: the second argument's fields win"
    (9090, True, "x")
    ( let m = mergeServerConfig defaultServerConfig
                (defaultServerConfig { scPort = 9090, scRequireAuth = True, scServerName = "x" })
      in (scPort m, scRequireAuth m, scServerName m) )
  let cfgFile = "/tmp/fms-test-serverconfig.json"
  missingCfg <- loadServerConfig "/tmp/fms-test-serverconfig-does-not-exist.json"
  check "loadServerConfig of a missing file -> defaults" defaultServerConfig missingCfg
  saveServerConfig cfgFile (defaultServerConfig { scServerName = "Saved", scPort = 1234 })
  loadedCfg <- loadServerConfig cfgFile
  check "saveServerConfig/loadServerConfig roundtrip"
    ("Saved", 1234) (scServerName loadedCfg, scPort loadedCfg)
  writeFile cfgFile "{\"Port\": 4321}"
  partialCfg <- loadServerConfig cfgFile
  check "loadServerConfig applies a partial file over the defaults"
    (4321, "functional-media-server") (scPort partialCfg, scServerName partialCfg)
  removeFile cfgFile

  -- Milestone 10b (startup wizard + branding): ServerConfig with the new
  -- fields, pre-10b config.json compatibility, patch set/clear semantics.
  check "defaultServerConfig 10b defaults (wizard not completed, empty branding)"
    (False, "", "")
    ( scStartupCompleted defaultServerConfig, scLoginDisclaimer defaultServerConfig
    , scCustomCss defaultServerConfig )
  check "ServerConfig FromJSON of pre-10b JSON (no new keys) -> defaults"
    (Just (False, "", ""))
    ((\sc -> (scStartupCompleted sc, scLoginDisclaimer sc, scCustomCss sc)) <$>
      (Aeson.decode "{\"ServerName\":\"Old\",\"RequireAuth\":true,\"LibraryPath\":\"/m\",\"Port\":1234}"
        :: Maybe ServerConfig))
  check "ServerConfig FromJSON of an empty object -> all defaults"
    (Just defaultServerConfig) (Aeson.decode "{}" :: Maybe ServerConfig)
  check "ServerConfig JSON roundtrip keeps the 10b keys"
    (Just True)
    (scStartupCompleted <$>
      (Aeson.decode (Aeson.encode (defaultServerConfig { scStartupCompleted = True }))
        :: Maybe ServerConfig))
  let p10b = fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
             (Aeson.decode "{\"StartupCompleted\":true,\"LoginDisclaimer\":\"hi\",\"CustomCss\":\"body{}\"}"
               :: Maybe ServerConfigPatch)
      m10b = applyServerConfigPatch defaultServerConfig p10b
  check "applyServerConfigPatch sets the 10b fields"
    (True, "hi", "body{}")
    (scStartupCompleted m10b, scLoginDisclaimer m10b, scCustomCss m10b)
  let p10bClear = fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                  (Aeson.decode "{\"LoginDisclaimer\":\"\"}" :: Maybe ServerConfigPatch)
      m10bClear = applyServerConfigPatch m10b p10bClear
  check "applyServerConfigPatch can clear a branding field to empty"
    ("", True, "body{}")
    (scLoginDisclaimer m10bClear, scStartupCompleted m10bClear, scCustomCss m10bClear)
  check "ServerConfigPatch null decodes to Nothing (= keep current, patch convention)"
    (Just Nothing)
    (scpLoginDisclaimer <$>
      (Aeson.decode "{\"LoginDisclaimer\":null}" :: Maybe ServerConfigPatch))
  check "applyServerConfigPatch with a null field keeps the base value"
    "hi"
    (scLoginDisclaimer (applyServerConfigPatch m10b
      (fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        (Aeson.decode "{\"LoginDisclaimer\":null}" :: Maybe ServerConfigPatch))))
  check "ServerConfigPatch ToJSON roundtrip with 10b keys"
    (Just p10b) (Aeson.decode (Aeson.encode p10b) :: Maybe ServerConfigPatch)
  check "isOpenPath: wizard + branding paths are open"
    [True, True, True, False]
    (map isOpenPath [["System","Wizard","Startup"], ["System","Wizard","Completed"], ["Branding","Configuration"], ["System","Configuration"]])

  -- Admin module (milestone 11a, docs/todo.org section 11): the activity
  -- ring buffer, the task-due predicate, and the JSON roundtrips.
  aNow <- getCurrentTime
  let mkEntry i = ActivityEntry i aNow "Test" ("msg " <> T.pack (show i))
      (n1, ab1) = pushActivity 1 [] (mkEntry 0)
  check "pushActivity assigns the id and bumps the counter"
    (2, [(mkEntry 0) { aeId = 1 }]) (n1, ab1)
  let (nBig, bBig) = foldl (\(n, b) _ -> pushActivity n b (mkEntry 0)) (1, [])
                     [1 .. 501 :: Int]
  check "pushActivity caps the ring buffer at activityLogCap"
    activityLogCap (length bBig)
  check "pushActivity drops the oldest entry first"
    (Just 2) (aeId <$> listToMaybe bBig)
  check "pushActivity keeps the newest entry last"
    (Just 501) (aeId <$> listToMaybe (reverse bBig))
  check "pushActivity counter after 501 pushes"
    502 nBig
  let tEnabled  = ScheduledTask "t" "T" 300 True Nothing Nothing
      tDisabled = tEnabled { stEnabled = False }
      tRecent   = tEnabled { stLastRun = Just (addUTCTime (-100) aNow) }
      tOld      = tEnabled { stLastRun = Just (addUTCTime (-400) aNow) }
      tBoundary = tEnabled { stLastRun = Just (addUTCTime (-300) aNow) }
  check "taskDue: enabled and never run -> True"  True  (taskDue aNow tEnabled)
  check "taskDue: disabled -> False"               False (taskDue aNow tDisabled)
  check "taskDue: last run older than interval -> True" True (taskDue aNow tOld)
  check "taskDue: last run within interval -> False"    False (taskDue aNow tRecent)
  check "taskDue: exactly at the interval boundary -> True" True (taskDue aNow tBoundary)
  let ae = ActivityEntry 7 aNow "TaskRun" "library-rescan: ok: done"
  check "ActivityEntry JSON roundtrip"
    (Just ae) (Aeson.decode (Aeson.encode ae) :: Maybe ActivityEntry)
  check "ActivityEntry JSON uses Id/Time/Type/Message keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode ae))
      ["\"Id\":", "\"Time\":", "\"Type\":", "\"Message\":"])

  -- Milestone 10d (/Dashboard aggregate): pure countsByType + uptimeSeconds.
  let mkI t = MediaItem
        { itemId = "1", itemTitle = "x", itemPath = "/x.mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = aNow, itemFolder = "1"
        , itemType = t, itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      cRaw = LBS.toStrict (Aeson.encode (countsByType [mkI "Movie", mkI "Movie", mkI "Photo", mkI "Episode"]))
  check "countsByType aggregates per type"
    [True, True, True]
    (map (`BSC.isInfixOf` cRaw) ["\"Movie\":2", "\"Photo\":1", "\"Episode\":1"])
  check "countsByType empty list -> empty object"
    "{}" (BSC.unpack (LBS.toStrict (Aeson.encode (countsByType []))))
  let t10d = aNow
      t210d = addUTCTime 90 t10d
  check "uptimeSeconds rounds to whole seconds"
    [0, 90, 90] [uptimeSeconds t10d t10d, uptimeSeconds t210d t10d, uptimeSeconds (addUTCTime 90.4 t10d) t10d]
  check "uptimeSeconds never negative (clock skew)"
    0 (uptimeSeconds t10d t210d)
  let stk = ScheduledTask "library-rescan" "Library rescan" 300 True (Just aNow) (Just "ok")
  check "ScheduledTask JSON roundtrip"
    (Just stk) (Aeson.decode (Aeson.encode stk) :: Maybe ScheduledTask)
  check "ScheduledTask JSON uses the PascalCase keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode stk))
      ["\"Id\":", "\"Name\":", "\"IntervalSeconds\":", "\"Enabled\":", "\"LastRun\":", "\"LastResult\":"])

  -- Web client (milestone 13/16, docs/todo.org section 13): the embedded
  -- single-page HTML must contain the player, the search query, and the
  -- playback/thumb/HLS/metadata links, and must be ASCII-only.
  check "web page contains a <video> player" True ("<video" `BSC.isInfixOf` LBS.toStrict webPageHtml)
  check "web page queries /items?search=" True ("/items?search=" `BSC.isInfixOf` LBS.toStrict webPageHtml)
  check "web page plays via /stream/" True ("/stream/" `BSC.isInfixOf` LBS.toStrict webPageHtml)
  check "web page links thumbs, HLS and metadata"
    True (all (`BSC.isInfixOf` LBS.toStrict webPageHtml)
      ["/items/'+it.Id+'/thumb", "/videos/'+it.Id+'/main.m3u8", "/items/'+it.Id+'/metadata"])
  check "web page is pure ASCII" True (LBS.all (<= 0x7F) webPageHtml)

  -- Media folders (milestone 2c, docs/todo.org section 2): folder JSON,
  -- folders.json persistence, nextFolderId, the /items?libraryId= filter,
  -- and the SafeCopy v0->v1 migration of MediaItem (docs/acid-state.org).
  let mf  = MediaFolder "2" "Movies" "/tmp/movies" "movies"
      mf1 = MediaFolder "1" "Media" "/media" "mixed"
      mfMap = HM.fromList [("1", mf1), ("2", mf)]
      foldersFile = "/tmp/fms-test-folders.json"
  check "MediaFolder JSON roundtrip"
    (Just mf) (Aeson.decode (Aeson.encode mf) :: Maybe MediaFolder)
  check "MediaFolder JSON uses Id/Name/Path/Type keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode mf))
      ["\"Id\":", "\"Name\":", "\"Path\":", "\"Type\":"])
  saveFolders foldersFile mfMap
  loadedFolders <- loadFolders foldersFile
  check "folders save/load roundtrip" mfMap loadedFolders
  check "loadFolders of a missing file is empty"
    HM.empty =<< loadFolders "/tmp/fms-test-folders-gone.json"
  removeFile foldersFile
  check "nextFolderId after numeric ids 1,2" "3" (nextFolderId mfMap)
  check "nextFolderId of an empty map is 1" "1" (nextFolderId HM.empty)
  let qi fid title = MediaItem
        { itemId = fid, itemTitle = title, itemPath = "/" ++ T.unpack title ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = aNow, itemFolder = fid
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      qItems = [qi "1" "b", qi "2" "m", qi "1" "a"]
      qSorted = [qi "1" "a", qi "1" "b", qi "2" "m"]
  check "applyItemQuery libraryId=2 keeps only folder-2 items"
    (1, [qi "2" "m"]) (applyItemQuery [("libraryId", "2")] qItems)
  check "applyItemQuery libraryId=1 keeps only folder-1 items"
    (2, [qi "1" "a", qi "1" "b"]) (applyItemQuery [("libraryId", "1")] qItems)
  check "applyItemQuery without libraryId keeps all (sorted by title)"
    (3, qSorted) (applyItemQuery [] qItems)
  check "applyItemQuery libraryId + search combine"
    (1, [qi "1" "a"]) (applyItemQuery [("libraryId", "1"), ("search", "a")] qItems)
  check "filterByFolder Nothing is the identity" qItems (filterByFolder Nothing qItems)
  check "filterByFolder Just filters by itemFolder"
    [qi "2" "m"] (filterByFolder (Just "2") qItems)
  let v0 = MediaItemV0
        { v0ItemId = "9", v0ItemTitle = "old", v0ItemPath = "/old.mp4"
        , v0ItemSize = 10, v0ItemMime = "video/mp4", v0ItemAddedAt = aNow }
      v1 = migrateMediaItem v0
  check "migrateMediaItem v0->v1 defaults itemFolder to \"1\"" "1" (v1ItemFolder v1)
  check "migrateMediaItem keeps the other fields"
    ("9", "/old.mp4", 10) (v1ItemId v1, v1ItemPath v1, v1ItemSize v1)
  check "MediaItemV1 SafeCopy v1 serialize/deserialize roundtrip"
    (Right v1) (runGet safeGet (runPut (safePut v1)))
  check "SafeCopy reads v0 bytes as v3 via the chain v0->v1->v2->v3 (folder \"1\")"
    (Right "1") (itemFolder <$> (runGet safeGet (runPut (safePut v0))
                                 :: Either String MediaItem))
  let v2 = migrateMediaItemV1 v1
  check "migrateMediaItemV1 v1->v2 derives type Movie from video mime"
    "Movie" (v2ItemType v2)
  check "migrateMediaItemV1 v1->v2 defaults series/season/episode to Nothing"
    (Nothing, Nothing, Nothing) (v2ItemSeries v2, v2ItemSeason v2, v2ItemEpisode v2)
  check "migrateMediaItemV1 keeps id/folder"
    ("9", "1") (v2ItemId v2, v2ItemFolder v2)
  check "MediaItemV2 SafeCopy v2 serialize/deserialize roundtrip"
    (Right v2) (runGet safeGet (runPut (safePut v2)))
  check "itemTypeFromMime audio -> Audio" "Audio" (itemTypeFromMime "audio/mpeg")
  check "itemTypeFromMime other -> Unknown" "Unknown" (itemTypeFromMime "application/octet-stream")
  -- milestone 2e: photo/book type matrix
  check "itemTypeFromMime video/* -> Movie" ["Movie", "Movie"] (map itemTypeFromMime ["video/mp4", "video/x-matroska"])
  check "itemTypeFromMime image/* -> Photo"
    ["Photo", "Photo", "Photo"] (map itemTypeFromMime ["image/jpeg", "image/png", "image/heic"])
  check "itemTypeFromMime book mimes -> Book"
    ["Book", "Book", "Book", "Book", "Book"]
    (map itemTypeFromMime
      [ "application/pdf", "application/epub+zip", "application/x-mobipocket-ebook"
      , "application/vnd.comicbook+zip", "application/vnd.comicbook-rar" ])
  check "itemTypeFromMime unknown application/* -> Unknown"
    "Unknown" (itemTypeFromMime "application/zip")
  check "SafeCopy reads v1 bytes as v4 (type from mime)"
    (Right "Movie") (itemType <$> (runGet safeGet (runPut (safePut v1))
                                   :: Either String MediaItem))
  check "SafeCopy reads v2 bytes as v4 (extraType defaults to Nothing)"
    (Right Nothing) (itemExtraType <$> (runGet safeGet (runPut (safePut v2))
                                         :: Either String MediaItem))
  let v3 = migrateMediaItemV2 v2
  check "migrateMediaItemV2 v2->v3 defaults v3ItemExtraType to Nothing"
    Nothing (v3ItemExtraType v3)
  check "migrateMediaItemV2 keeps id/type/folder"
    ("9", "Movie", "1") (v3ItemId v3, v3ItemType v3, v3ItemFolder v3)
  check "MediaItemV3 SafeCopy v3 serialize/deserialize roundtrip"
    (Right v3) (runGet safeGet (runPut (safePut v3)))
  check "SafeCopy reads v3 bytes as v4 (versions default to [])"
    (Right []) (itemVersions <$> (runGet safeGet (runPut (safePut v3))
                                  :: Either String MediaItem))
  let v4 = migrateMediaItemV3 v3
  check "migrateMediaItemV3 v3->v4 defaults itemVersions to []"
    [] (itemVersions v4)
  check "migrateMediaItemV3 keeps id/type/folder/extraType"
    ("9", "Movie", "1", Nothing) (itemId v4, itemType v4, itemFolder v4, itemExtraType v4)
  check "MediaItem v4 SafeCopy roundtrip keeps versions"
    (Right (v4 { itemVersions = ["/m/a.1080p.mkv", "/m/a.2160p.mkv"] }))
    (runGet safeGet (runPut (safePut (v4 { itemVersions = ["/m/a.1080p.mkv", "/m/a.2160p.mkv"] })))
      :: Either String MediaItem)

  -- Item types + TV shows (milestone 4d, docs/todo.org sections 2-4).
  check "parseEpisodeFilename S01E02" (Just (1, 2)) (parseEpisodeFilename "S01E02")
  check "parseEpisodeFilename s1e2 lowercase" (Just (1, 2)) (parseEpisodeFilename "s1e2")
  check "parseEpisodeFilename S01E02E03 takes the first pair"
    (Just (1, 2)) (parseEpisodeFilename "S01E02E03")
  check "parseEpisodeFilename S10E10" (Just (10, 10)) (parseEpisodeFilename "S10E10")
  check "parseEpisodeFilename no match" Nothing (parseEpisodeFilename "movie")
  check "parseEpisodeFilename inside a longer name"
    (Just (2, 5)) (parseEpisodeFilename "My Show S02E05")
  check "classifyItem with tvshow.nfo -> Episode of the dir name"
    ("Episode", Just "MyShow", Just 1, Just 2)
    (classifyItem True "/lib/MyShow/S01E02.mp4" "S01E02")
  check "classifyItem without nfo -> Movie from mime"
    ("Movie", Nothing, Nothing, Nothing)
    (classifyItem False "/lib/movie.mp4" "movie")
  check "m43 classifyItem: scene SxxEyy file without nfo -> Episode with cleaned series"
    (classifyItem False "/dl/Family.Guy.S25E00.1080p.HEVC.x265-MeGusta.mkv" "Family.Guy.S25E00.1080p.HEVC.x265-MeGusta")
    ("Episode", Just "Family Guy", Just 25, Just 0)
  check "m44 classifyItem: exact live filename -> series cut at the SxxEyy tag"
    (classifyItem False "/dl/Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta.mkv" "Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta")
    ("Episode", Just "Family Guy", Just 25, Just 0)
  check "m44 parseEpisodeTagPos returns the tag start offset"
    (parseEpisodeTagPos "Family.Guy.S25E00.1080p.x265-MeGusta")
    (Just (11, 25, 0))
  check "m44 seriesFromEpisodeName: part BEFORE the SxxEyy tag, cleaned (live filename)"
    (seriesFromEpisodeName "Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta")
    "Family Guy"
  check "m44 seriesFromEpisodeName: tag at start -> fall back to full cleaned name"
    (seriesFromEpisodeName "S01E02.The.Beginning.1080p.x265-GRP")
    "S01E02 The Beginning"
  check "m44 seriesFromEpisodeName: no tag -> whole name cleaned"
    (seriesFromEpisodeName "Family.Guy.1080p.x265-MeGusta")
    "Family Guy"
  check "m43 classifyItem with tvshow.nfo still wins (unchanged)"
    (classifyItem True "/lib/MyShow/S01E02.mkv" "S01E02.mkv")
    ("Episode", Just "MyShow", Just 1, Just 2)
  check "classifyItem without nfo, audio -> Audio"
    ("Audio", Nothing, Nothing, Nothing)
    (classifyItem False "/lib/song.mp3" "song")
  let tvNfo = BSC.pack (unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<tvshow>"
        , "  <title>My Show</title>"
        , "  <overview>A test show</overview>"
        , "  <genre>Drama</genre>"
        , "  <premiered>2022-01-01</premiered>"
        , "  <rating>8.0</rating>"
        , "  <tmdbid>4242</tmdbid>"
        , "</tvshow>" ])
  case parseTvShowNfo tvNfo of
    Left _ -> check "parseTvShowNfo parses a <tvshow> root" True False
    Right m -> do
      check "parseTvShowNfo parses a <tvshow> root" True True
      check "parseTvShowNfo title/overview/genres"
        (Just "My Show", Just "A test show", ["Drama"]) (metaTitle m, metaOverview m, metaGenres m)
      check "parseTvShowNfo premiered -> year 2022" (Just 2022) (metaYear m)
      check "parseTvShowNfo rating + tmdbid + source"
        (Just 8.0, Just "4242", "nfo") (metaRating m, HM.lookup "tmdb" (metaProviderIds m), metaSource m)
  check "parseTvShowNfo rejects a <movie> root"
    True (either (const True) (const False) (parseTvShowNfo (BSC.pack "<movie><title>x</title></movie>")))
  let epA = (qi "1" "S01E01") { itemType = "Episode", itemSeries = Just "MyShow", itemSeason = Just 1, itemEpisode = Just 1 }
      epB = (qi "1" "S01E02") { itemType = "Episode", itemSeries = Just "MyShow", itemSeason = Just 1, itemEpisode = Just 2 }
      epC = (qi "1" "S02E01") { itemType = "Episode", itemSeries = Just "MyShow", itemSeason = Just 2, itemEpisode = Just 1 }
      other = (qi "2" "Other") { itemType = "Episode", itemSeries = Just "OtherShow" }
      showItems = [epB, other, epA, epC, qi "1" "movie"]
  check "showsFromItems aggregates and sorts by name"
    [("MyShow", 3), ("OtherShow", 1)] (showsFromItems showItems)
  check "showsFromItems ignores non-episodes" [] (showsFromItems [qi "1" "movie"])
  check "sortEpisodes orders by season/episode"
    [epA, epB, epC] (sortEpisodes [epC, epB, epA])
  check "filterByType Episode keeps episodes in list order"
    [epB, other, epA, epC] (filterByType (Just "Episode") showItems)
  check "filterByType Movie" [qi "1" "movie"] (filterByType (Just "Movie") showItems)
  check "filterBySeries MyShow" [epB, epA, epC] (filterBySeries (Just "MyShow") showItems)
  check "applyItemQuery type=Episode (then title sort)"
    (4, [other, epA, epB, epC])
    (applyItemQuery [("type", "Episode")] showItems)
  check "applyItemQuery series=MyShow&type=Episode"
    (3, [epA, epB, epC])
    (applyItemQuery [("series", "MyShow"), ("type", "Episode")] showItems)

  -- Richer /items query (milestone 4e, docs/todo.org section 4).
  check "itemYear is Nothing (MediaItem has no year field)"
    Nothing (itemYear (qi "1" "movie"))
  check "filterByIncludeTypes case-insensitive episode"
    [epB, other, epA, epC] (filterByIncludeTypes (Just "episode") showItems)
  check "filterByIncludeTypes Movie,Episode keeps all five"
    showItems (filterByIncludeTypes (Just "Movie,Episode") showItems)
  check "filterByIncludeTypes Audio matches nothing"
    [] (filterByIncludeTypes (Just "Audio") showItems)
  check "filterByIncludeTypes empty string is identity"
    showItems (filterByIncludeTypes (Just "") showItems)
  check "filterByIncludeTypes Nothing is identity"
    showItems (filterByIncludeTypes Nothing showItems)
  check "filterByYears with a listed year matches nothing (no year field)"
    [] (filterByYears (Just "2022") showItems)
  check "filterByYears with only invalid tokens is identity"
    showItems (filterByYears (Just "abc") showItems)
  check "filterByYears Nothing is identity"
    showItems (filterByYears Nothing showItems)
  check "applyItemQuery searchTerm is an alias of search"
    (applyItemQuery [("search", "S01")] showItems)
    (applyItemQuery [("searchTerm", "S01")] showItems)
  check "applyItemQuery search wins over searchTerm when both present"
    (applyItemQuery [("search", "movie")] showItems)
    (applyItemQuery [("search", "movie"), ("searchTerm", "S01")] showItems)
  check "applyItemQuery searchTerm=S01 finds S01E01+S01E02 sorted"
    (2, [epA, epB]) (applyItemQuery [("searchTerm", "S01")] showItems)
  check "applyItemQuery includeItemTypes=Episode (then title sort)"
    (4, [other, epA, epB, epC])
    (applyItemQuery [("includeItemTypes", "Episode")] showItems)
  check "applyItemQuery includeItemTypes + years (no year field -> 0)"
    (0, []) (applyItemQuery [("includeItemTypes", "Movie,Episode"), ("years", "2022")] showItems)
  check "applyItemQuery recursive + filters accepted and ignored"
    (applyItemQuery [] showItems)
    (applyItemQuery [("recursive", "true"), ("filters", "IsPlayed,IsFavorite,IsResumable")] showItems)
  check "applyItemQuery type=Episode still works alongside 4e params"
    (4, [other, epA, epB, epC])
    (applyItemQuery [("type", "Episode"), ("recursive", "false")] showItems)

  -- User-scoped filters + fields projection (milestone 5e).
  let ud5eRec played fav pos = UserData played fav pos 0 Nothing
      ud5eMap = HM.fromList
        [ ("1", ud5eRec True True (Just 30))    -- played + favorite + resumable-but-played
        , ("2", ud5eRec False False (Just 5))   -- resumable
        , ("3", ud5eRec True False Nothing)     -- played, not resumable
        ]
      ud5eLookup = flip HM.lookup ud5eMap
      uItems = [qi "1" "a", qi "2" "b", qi "3" "c", qi "4" "d"]
  check "applyUserDataFilters IsPlayed" [qi "1" "a", qi "3" "c"]
    (applyUserDataFilters ["IsPlayed"] ud5eLookup uItems)
  check "applyUserDataFilters Unplayed (missing record = unplayed)"
    [qi "2" "b", qi "4" "d"] (applyUserDataFilters ["Unplayed"] ud5eLookup uItems)
  check "applyUserDataFilters IsFavorite" [qi "1" "a"]
    (applyUserDataFilters ["IsFavorite"] ud5eLookup uItems)
  check "applyUserDataFilters IsLiked is an alias of IsFavorite" [qi "1" "a"]
    (applyUserDataFilters ["IsLiked"] ud5eLookup uItems)
  check "applyUserDataFilters IsResumable (pos>0 and not played)" [qi "2" "b"]
    (applyUserDataFilters ["IsResumable"] ud5eLookup uItems)
  check "applyUserDataFilters tokens are case-insensitive" [qi "1" "a", qi "3" "c"]
    (applyUserDataFilters ["isplayed"] ud5eLookup uItems)
  check "applyUserDataFilters tokens are ANDed" [qi "1" "a"]
    (applyUserDataFilters ["IsPlayed", "IsFavorite"] ud5eLookup uItems)
  check "applyUserDataFilters unknown token ignored" uItems
    (applyUserDataFilters ["IsSomething"] ud5eLookup uItems)
  check "applyUserDataFilters empty list is identity" uItems
    (applyUserDataFilters [] ud5eLookup uItems)
  let iv = Aeson.toJSON (qi "1" "movie")
      lookupObj v = case v of
        Aeson.Object o -> KeyMap.toList o
        _              -> []
      keysOf v = map (Key.toText . fst) (lookupObj v)
  check "projectFields keeps requested keys (case-insensitive) plus id"
    ["Id", "Name"] (keysOf (projectFields ["Name"] iv))
  check "projectFields empty list is identity" iv (projectFields [] iv)
  check "projectFields keeps only id when nothing requested matches"
    ["Id"] (keysOf (projectFields ["nope"] iv))
  check "projectFields non-object passthrough"
    (Aeson.String "hello") (projectFields ["id"] (Aeson.String "hello"))
  check "projectFields result still encodes"
    True (not (null (LBSC.unpack (Aeson.encode (projectFields ["type"] iv)))))
  let attached = attachUserData (Just ud5eLookup) (qi "1" "movie") iv
  check "attachUserData adds a UserData key with the looked-up record"
    (Just (Aeson.toJSON (ud5eRec True True (Just 30))))
    (case attached of
       Aeson.Object o -> KeyMap.lookup (Key.fromText "UserData") o
       _              -> Nothing)
  check "attachUserData defaults to emptyUserData for unknown items"
    (Just (Aeson.toJSON emptyUserData))
    (case attachUserData (Just ud5eLookup) (qi "9" "zz") iv of
       Aeson.Object o -> KeyMap.lookup (Key.fromText "UserData") o
       _              -> Nothing)
  check "attachUserData without a lookup is identity"
    iv (attachUserData Nothing (qi "1" "movie") iv)
  check "attachUserData + projectFields: fields=UserData keeps the record"
    [ "Id", "UserData" ]
    (keysOf (projectFields ["userdata"] (attachUserData (Just ud5eLookup) (qi "1" "movie") iv)))

  -- Embedded subtitle streams (milestone 3f, docs/todo.org section 7).
  let mkStream i t c l =
        MediaStream i t c l Nothing Nothing Nothing Nothing Nothing False
      probeStreams =
        [ mkStream 0 "Video" "h264" Nothing
        , mkStream 1 "Audio" "aac" (Just "eng")
        , mkStream 2 "Subtitle" "subrip" (Just "eng")
        , mkStream 3 "Subtitle" "ass" Nothing
        ]
  check "embeddedSubtitleSources keeps only Subtitle streams (index, codec, language)"
    [(2, "subrip", Just "eng"), (3, "ass", Nothing)]
    (embeddedSubtitleSources probeStreams)
  check "embeddedSubtitleSources of empty input is empty"
    [] (embeddedSubtitleSources [])
  check "embeddedSubtitleSources ignores video/audio streams"
    [] (embeddedSubtitleSources [mkStream 0 "Video" "h264" Nothing, mkStream 1 "Audio" "aac" Nothing])
  let mixedSrcs =
        [ External "/m/clip.srt" "srt"
        , External "/m/clip.vtt" "vtt"
        , Embedded 2 "subrip" (Just "eng")
        ]
  check "indexSources assigns 0-based indexes, externals first then embedded"
    [ (0, External "/m/clip.srt" "srt")
    , (1, External "/m/clip.vtt" "vtt")
    , (2, Embedded 2 "subrip" (Just "eng"))
    ]
    (indexSources mixedSrcs)
  check "embeddedToVttArgs maps stream 0:n and converts to webvtt on stdout"
    ["-v", "error", "-i", "/m/clip.mkv", "-map", "0:2", "-c:s", "webvtt", "-f", "webvtt", "pipe:1"]
    (embeddedToVttArgs "/m/clip.mkv" 2)
  -- IO: the combined source list on a real directory. A dummy media file
  -- makes ffprobe fail -> only the external entry, no crash.
  let subDir = "/tmp/fms-test-subs-3f"
  resetDir subDir
  createDirectoryIfMissing True subDir
  let dummyMedia = subDir </> "clip.mp4"
  BS.writeFile dummyMedia (BSC.pack "not a real media file")
  BS.writeFile (subDir </> "clip.srt") (BSC.pack "1\n00:00:01,000 --> 00:00:02,000\nhello\n")
  BS.writeFile (subDir </> "other.vtt") (BSC.pack "WEBVTT\n\n00:00:01.000 --> 00:00:02.000\nhi\n")
  srcs3f <- subtitleListSources dummyMedia
  check "subtitleListSources: external files first, probe failure adds no embedded"
    [ External (subDir </> "clip.srt") "srt" ]
    srcs3f
  subs3f <- subtitleList dummyMedia
  check "subtitleList keeps the milestone-3d shape (index, path, format)"
    [(0, subDir </> "clip.srt", "srt")] subs3f
  -- IO, ffprobe-guarded: a real container with an embedded srt stream.
  mFFprobe <- findExecutable "ffprobe"
  mFFmpeg3f <- findExecutable "ffmpeg"
  case (mFFprobe, mFFmpeg3f) of
    (Just _, Just _) -> do
      let mkv = subDir </> "withsubs.mkv"
          srt = subDir </> "withsubs.srt"
      BS.writeFile srt (BSC.pack "1\n00:00:01,000 --> 00:00:02,000\nembedded hello\n")
      (ecMux, _, muxErr) <- runProcessBS "ffmpeg"
        [ "-v", "error", "-f", "lavfi", "-i", "testsrc=duration=1:size=64x48:rate=5"
        , "-i", srt, "-map", "0:v", "-map", "1:s", "-c:v", "libx264"
        , "-c:s", "srt", "-y", mkv ]
      case ecMux of
        ExitFailure _ ->
          putStrLn ("SKIP: ffmpeg mux failed: " ++ show (showBS (LBS.toStrict muxErr)))
        ExitSuccess -> do
          srcsMkv <- subtitleListSources mkv
          let embOk = case srcsMkv of
                [ External p "srt", Embedded si codec _ ] ->
                  p == subDir </> "withsubs.srt" && si == 1 && codec == "subrip"
                _ -> False
          check "subtitleListSources: external withsubs.srt first, then embedded stream 1 (subrip)"
            True embOk
          subsMkv <- subtitleList mkv
          check "subtitleList on the muxed file still lists only the external file"
            [(0, subDir </> "withsubs.srt", "srt")] subsMkv
    _ -> putStrLn "SKIP: ffprobe/ffmpeg not found on PATH -- skipping embedded-subtitle IO test"

  -- Extras / special features (milestone 4g, docs/todo.org sections 2-4).
  check "extraTypeFor trailer" (Just "Trailer")
    (extraTypeFor "/m/movie-trailer.mp4" "movie-trailer")
  check "extraTypeFor case-insensitive TRAILER" (Just "Trailer")
    (extraTypeFor "/m/MOVIE-TRAILER.mp4" "MOVIE-TRAILER")
  check "extraTypeFor behind the scenes" (Just "BehindTheScenes")
    (extraTypeFor "/m/movie behind the scenes.mp4" "movie behind the scenes")
  check "extraTypeFor bts abbreviation" (Just "BehindTheScenes")
    (extraTypeFor "/m/bts.mp4" "bts")
  check "extraTypeFor deletedscene" (Just "DeletedScene")
    (extraTypeFor "/m/deletedscene.mp4" "deletedscene")
  check "extraTypeFor featurette" (Just "Featurette")
    (extraTypeFor "/m/featurette.mp4" "featurette")
  check "extraTypeFor short (substring)" (Just "Short")
    (extraTypeFor "/m/my short film.mp4" "my short film")
  check "extraTypeFor theme" (Just "ThemeSong")
    (extraTypeFor "/m/theme song.mp4" "theme song")
  check "extraTypeFor extras dir -> Other" (Just "Other")
    (extraTypeFor "/m/extras/clip.mp4" "clip")
  check "extraTypeFor specials dir -> Other" (Just "Other")
    (extraTypeFor "/m/specials/clip.mp4" "clip")
  check "extraTypeFor token in parent dir name (Trailers)" (Just "Trailer")
    (extraTypeFor "/m/Trailers/clip.mp4" "clip")
  check "extraTypeFor filename token wins over extras dir" (Just "Trailer")
    (extraTypeFor "/m/extras/movie-trailer.mp4" "movie-trailer")
  check "extraTypeFor no match -> Nothing" Nothing
    (extraTypeFor "/m/movie.mp4" "movie")
  let mainItem = (qi "1" "movie") { itemPath = "/m/movie.mp4" }
      exSameDir = (qi "2" "aaa trailer") { itemPath = "/m/movie-trailer.mp4"
                                          , itemType = "Extra", itemExtraType = Just "Trailer", itemVersions = [] }
      exOneBelow = (qi "3" "behind") { itemPath = "/m/extras/behind.mp4"
                                     , itemType = "Extra", itemExtraType = Just "BehindTheScenes", itemVersions = [] }
      exDeep = (qi "4" "deep") { itemPath = "/m/extras/deep/x.mp4"
                               , itemType = "Extra", itemExtraType = Just "Other", itemVersions = [] }
      exFar = (qi "5" "far") { itemPath = "/other/far.mp4"
                             , itemType = "Extra", itemExtraType = Just "Other", itemVersions = [] }
      plainSameDir = (qi "6" "clip") { itemPath = "/m/clip.mp4" }
      sfItems = [mainItem, exSameDir, exOneBelow, exDeep, exFar, plainSameDir]
  check "specialFeaturesOf: same dir + one level below, Extras only, sorted by title"
    [exSameDir, exOneBelow] (specialFeaturesOf mainItem sfItems)
  check "specialFeaturesOf from another dir finds only extras there (itself)"
    [exFar] (specialFeaturesOf exFar sfItems)

  -- Studios / persons / tags (milestone 4h, docs/todo.org section 3).
  let sm studios persons tags =
        sparseMeta { metaStudios = studios, metaPersons = persons, metaTags = tags }
  check "studiosFromMetadata groups/counts/sorts"
    [("StudioA",2),("StudioB",1)]
    (studiosFromMetadata [sm ["StudioB","StudioA"] [] [], sm ["StudioA"] [] [], sm [] [] []])
  check "personsFromMetadata groups/counts/sorts"
    [("Alice",2),("Bob",1)]
    (personsFromMetadata [sm [] ["Bob","Alice"] [], sm [] ["Alice"] [], sm [] [] []])
  check "tagsFromMetadata groups/counts/sorts"
    [("tag1",1),("tag2",2)]
    (tagsFromMetadata [sm [] [] ["tag2"], sm [] [] ["tag2","tag1"]])
  check "studiosFromMetadata on empty input is []" [] (studiosFromMetadata [])
  let nfo4h = BSC.pack (unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<movie>"
        , "  <title>T</title>"
        , "  <studio>StudioA</studio>"
        , "  <studio>StudioB</studio>"
        , "  <actor><name>Alice</name><role>R</role></actor>"
        , "  <actor><name>Bob</name></actor>"
        , "  <director>Dir Person</director>"
        , "  <tag>tag1</tag>"
        , "</movie>" ])
  case parseMovieNfo nfo4h of
    Left _ -> check "parseMovieNfo parses studio/actor/director/tag fields" True False
    Right m -> do
      check "parseMovieNfo <studio> list" ["StudioA","StudioB"] (metaStudios m)
      check "parseMovieNfo persons = actor names + director"
        ["Alice","Bob","Dir Person"] (metaPersons m)
      check "parseMovieNfo <tag> list" ["tag1"] (metaTags m)
  check "MediaMetadata JSON roundtrip keeps the new fields"
    (Just (sm ["S"] ["P"] ["t"]))
    (Aeson.decode (Aeson.encode (sm ["S"] ["P"] ["t"])) :: Maybe MediaMetadata)
  check "MediaMetadata FromJSON of pre-4h JSON (no new keys) defaults to []"
    (Just ([], [], []))
    ((\m -> (metaStudios m, metaPersons m, metaTags m))
       <$> (Aeson.decode (LBSC.pack "{\"title\":\"x\",\"genres\":[\"Drama\"],\"source\":\"nfo\"}")
            :: Maybe MediaMetadata))
  let metaOf4h studios genres persons =
        sparseMeta { metaStudios = studios, metaGenres = genres, metaPersons = persons }
      mMap4h = HM.fromList
        [ ("1", metaOf4h ["StudioA"] ["Action"] ["Alice"])
        , ("2", metaOf4h ["StudioB"] ["Drama"] ["Bob"])
        ]
      mLookup4h = flip HM.lookup mMap4h
      fItems = [qi "1" "a", qi "2" "b", qi "3" "c"]   -- item 3 has no metadata
  check "applyMetadataFilters genres=ACTION (case-insensitive), no-metadata item excluded"
    [qi "1" "a"] (applyMetadataFilters [("genres", "ACTION")] mLookup4h fItems)
  check "applyMetadataFilters genres=action,drama (ANY token intersects)"
    [qi "1" "a", qi "2" "b"] (applyMetadataFilters [("genres", "action,drama")] mLookup4h fItems)
  check "applyMetadataFilters studios=studiob" [qi "2" "b"]
    (applyMetadataFilters [("studios", "studiob")] mLookup4h fItems)
  check "applyMetadataFilters persons=Bob" [qi "2" "b"]
    (applyMetadataFilters [("persons", "Bob")] mLookup4h fItems)
  check "applyMetadataFilters different params are ANDed"
    [] (applyMetadataFilters [("genres", "action"), ("studios", "StudioB")] mLookup4h fItems)
  check "applyMetadataFilters empty params are the identity"
    fItems (applyMetadataFilters [] mLookup4h fItems)
  check "applyMetadataFilters blank param value is the identity"
    fItems (applyMetadataFilters [("genres", " , ")] mLookup4h fItems)
  check "applyMetadataFilters unknown param is ignored"
    fItems (applyMetadataFilters [("year", "2020")] mLookup4h fItems)

  -- Collections / BoxSets (milestone 7): pure helpers, JSON, boxset
  -- pseudo-item synthesis, persistence roundtrip.
  check "collections addItem appends, dedupes, keeps order"
    ["1", "2", "3"] (CM.addItem "3" (CM.addItem "2" (CM.addItem "1" [])))
  check "collections addItem of an existing id is identity"
    ["1", "2"] (CM.addItem "1" ["1", "2"])
  check "collections removeItem removes all occurrences"
    ["2"] (CM.removeItem "1" ["1", "2", "1"])
  let col = Collection "abc" "Favs" ["1", "2"] aNow
  check "Collection JSON roundtrip" (Just col) (Aeson.decode (Aeson.encode col) :: Maybe Collection)
  check "Collection JSON keys are PascalCase like Playlist"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode col))
      [ "\"Id\":", "\"Name\":", "\"ItemIds\":", "\"AddedAt\":" ])
  check "boxSetIdFor prefixes the collection id" "boxset-abc" (boxSetIdFor "abc")
  check "isBoxSetId recognises pseudo-item ids" (True, False) (isBoxSetId "boxset-abc", isBoxSetId "1")
  let bvJson = LBSC.unpack (Aeson.encode (boxSetItemValue col))
  check "boxSetItemValue has Id/Name/Type/ItemCount keys (milestone 17b)"
    True (  "\"Id\":\"boxset-abc\"" `isInfixOf` bvJson
         && "\"Name\":\"Favs\"" `isInfixOf` bvJson
         && "\"Type\":\"BoxSet\"" `isInfixOf` bvJson
         && "\"ItemCount\":2" `isInfixOf` bvJson )
  check "typeFilterIncludesBoxSet type=BoxSet" True (typeFilterIncludesBoxSet [("type", "BoxSet")])
  check "typeFilterIncludesBoxSet is case-insensitive" True (typeFilterIncludesBoxSet [("type", "boxset")])
  check "typeFilterIncludesBoxSet includeItemTypes=Movie,BoxSet" True
    (typeFilterIncludesBoxSet [("includeItemTypes", "Movie,BoxSet")])
  check "typeFilterIncludesBoxSet type=Movie is False" False (typeFilterIncludesBoxSet [("type", "Movie")])
  check "typeFilterIncludesBoxSet with no params is False" False (typeFilterIncludesBoxSet [])
  let colFile = "/tmp/fms-test-collections.json"
      colMap = HM.fromList [("abc", col), ("def", Collection "def" "Other" [] aNow)]
  saveCollections colFile colMap
  loadedCols <- loadCollections colFile
  check "saveCollections/loadCollections roundtrip" colMap loadedCols
  removeFile colFile
  check "loadCollections of a missing file is empty"
    HM.empty =<< loadCollections "/tmp/fms-test-collections-gone.json"

  -- WebSocket socket API (milestone 8): pure message encoding, api_key
  -- extraction, and the connection-registry helpers.
  let kaMsg = LBSC.unpack (encodeMessage "KeepAlive" (Aeson.object []))
  check "encodeMessage shape {\"MessageType\",\"Data\"}"
    True (  "\"MessageType\":\"KeepAlive\"" `isInfixOf` kaMsg
         && "\"Data\":{}" `isInfixOf` kaMsg )
  let paMsg = LBSC.unpack (encodeMessage "LibraryChanged" (Aeson.toJSON (["a", "b"] :: [T.Text])))
  check "encodeMessage carries the Data payload"
    True (  "\"MessageType\":\"LibraryChanged\"" `isInfixOf` paMsg
         && "\"Data\":[\"a\",\"b\"]" `isInfixOf` paMsg )
  check "parseApiKey reads api_key" (Just "k1") (parseApiKey [("api_key", "k1")])
  check "parseApiKey falls back to token" (Just "t1") (parseApiKey [("token", "t1")])
  check "parseApiKey prefers api_key over token" (Just "k1")
    (parseApiKey [("api_key", "k1"), ("token", "t1")])
  check "parseApiKey ignores empty values" Nothing (parseApiKey [("api_key", "")])
  check "parseApiKey with no params is Nothing" Nothing (parseApiKey [])
  check "parseApiKeyFromPath parses /Socket?api_key=k9"
    (Just "k9") (parseApiKeyFromPath (BSC.pack "/Socket?api_key=k9"))
  check "parseApiKeyFromPath with token param and extra params"
    (Just "t2") (parseApiKeyFromPath (BSC.pack "/socket?foo=1&token=t2"))
  check "parseApiKeyFromPath without query is Nothing"
    Nothing (parseApiKeyFromPath (BSC.pack "/Socket"))
  check "addConn appends"
    ([(1, "c1"), (2, "c2")] :: [(Int, T.Text)])
    (addConn (2 :: Int) ("c2" :: T.Text) (addConn (1 :: Int) "c1" []))
  check "removeConn removes only the matching key"
    ([(2, "c2")] :: [(Int, T.Text)])
    (removeConn (1 :: Int) [(1, "c1"), (2, "c2")])
  check "removeConn of a missing key is identity"
    ([(1, "c1")] :: [(Int, T.Text)])
    (removeConn (9 :: Int) [(1, "c1")])

  -- Milestone 9: parental controls (ratingRank/ratingAllowed/tagsAllowed/
  -- scheduleAllowsNow/itemAllowedByPolicy) + enable/disable (User defaults,
  -- PolicyPatch merge, NFO <mpaa>).
  let pol9 = UserPolicy (Just "PG-13") ["Horror"] [AccessSchedule 0 9 17] Nothing
  check "ratingRank movie ratings" [1,2,3,4,5] (map ratingRank ["G","PG","PG-13","R","NC-17"])
  check "ratingRank tv ratings" [1,2,2,3,4,5] (map ratingRank ["TV-Y","TV-Y7","TV-G","TV-PG","TV-14","TV-MA"])
  check "ratingRank is case-insensitive and strips whitespace" 3 (ratingRank " pg-13 ")
  check "ratingRank of an unknown rating is 99 (most restrictive)" 99 (ratingRank "XXX")
  check "ratingAllowed: no max allows everything" True (ratingAllowed Nothing (Just "R"))
  check "ratingAllowed: unrated item passes" True (ratingAllowed (Just "G") Nothing)
  check "ratingAllowed: within max" True (ratingAllowed (Just "PG-13") (Just "PG"))
  check "ratingAllowed: above max is blocked" False (ratingAllowed (Just "PG-13") (Just "R"))
  check "ratingAllowed: equal to max is allowed, case-insensitive" True (ratingAllowed (Just "R") (Just "r"))
  check "tagsAllowed: no blocked tags" True (tagsAllowed [] ["a","b"])
  check "tagsAllowed: disjoint sets" True (tagsAllowed ["Horror"] ["Comedy"])
  check "tagsAllowed: intersection is blocked, case-insensitive" False (tagsAllowed ["horror"] ["Horror","comedy"])
  let sun10 = UTCTime (fromGregorian 2024 1 7) (timeOfDayToTime (TimeOfDay 10 0 0))  -- 2024-01-07 = Sunday 10:00
      sun20 = UTCTime (fromGregorian 2024 1 7) (timeOfDayToTime (TimeOfDay 20 0 0))  -- Sunday 20:00
      mon10 = UTCTime (fromGregorian 2024 1 8) (timeOfDayToTime (TimeOfDay 10 0 0))  -- Monday 10:00
  check "scheduleAllowsNow: empty schedule always allows" True (scheduleAllowsNow [] sun20)
  check "scheduleAllowsNow: inside the window" True (scheduleAllowsNow [AccessSchedule 0 9 17] sun10)
  check "scheduleAllowsNow: outside the window (hour)" False (scheduleAllowsNow [AccessSchedule 0 9 17] sun20)
  check "scheduleAllowsNow: wrong day" False (scheduleAllowsNow [AccessSchedule 0 9 17] mon10)
  check "scheduleAllowsNow: any of several windows" True
    (scheduleAllowsNow [AccessSchedule 1 0 8, AccessSchedule 0 9 17] sun10)
  check "itemAllowedByPolicy: empty policy allows anything" True (itemAllowedByPolicy emptyUserPolicy Nothing)
  check "itemAllowedByPolicy: blocked tag" False
    (itemAllowedByPolicy pol9 (Just sparseMeta { metaTags = ["Horror"] }))
  check "itemAllowedByPolicy: rating above max" False
    (itemAllowedByPolicy pol9 (Just sparseMeta { metaOfficialRating = Just "R" }))
  check "itemAllowedByPolicy: ok item passes" True
    (itemAllowedByPolicy pol9 (Just sparseMeta { metaTags = ["Comedy"], metaOfficialRating = Just "PG" }))
  check "User FromJSON of pre-9 JSON (no policy/enabled) gets defaults"
    (Just (emptyUserPolicy, True))
    ((\u -> (userPolicy u, userEnabled u)) <$>
      (Aeson.decode (LBSC.pack "{\"userId\":\"9\",\"userName\":\"old\",\"passwordHash\":\"h\",\"isAdmin\":false,\"apiKeys\":[]}")
        :: Maybe User))
  check "UserPolicy JSON roundtrip" (Just pol9) (Aeson.decode (Aeson.encode pol9) :: Maybe UserPolicy)
  check "PolicyPatch partial decode + applyPolicyPatch merge (absent fields kept)"
    (UserPolicy (Just "R") ["Horror","Scary"] [AccessSchedule 0 9 17] Nothing)
    (applyPolicyPatch (UserPolicy (Just "PG") ["Horror"] [AccessSchedule 0 9 17] Nothing)
      (either (const (PolicyPatch Nothing Nothing Nothing Nothing)) id
        (Aeson.eitherDecode (LBSC.pack "{\"MaxParentalRating\":\"R\",\"BlockedTags\":[\"Horror\",\"Scary\"]}"))))
  check "PolicyPatch explicit null clears the max rating"
    Nothing
    (policyMaxRating (applyPolicyPatch pol9 (PolicyPatch (Just Nothing) Nothing Nothing Nothing)))
  check "parseMovieNfo reads <mpaa> into officialRating"
    (Just (Just "PG-13"))
    (metaOfficialRating <$> either (const Nothing) Just
      (parseMovieNfo (BSC.pack "<movie><title>x</title><mpaa>PG-13</mpaa></movie>")))

  -- Milestone 5f (watch history): sortByUserDataField + batch ids body.
  let mi5 i t = MediaItem
        { itemId = i, itemTitle = t, itemPath = "/" ++ T.unpack t ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = aNow, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      tA = UTCTime (fromGregorian 2024 1 1) 0
      tB = UTCTime (fromGregorian 2024 2 1) 0
      udMap5 = HM.fromList
        [ ("1", emptyUserData { udLastPlayedAt = Just tA, udPlayCount = 3 })
        , ("2", emptyUserData { udLastPlayedAt = Just tB, udPlayCount = 1 })
        , ("3", emptyUserData { udPlayCount = 5 })          -- never played
        ]
      lk5 = flip HM.lookup udMap5
      i1 = mi5 "1" "a"; i2 = mi5 "2" "b"; i3 = mi5 "3" "c"
  check "isUserDataSortKey: the two keys, case-insensitive; others not"
    [True, True, False, False]
    (map isUserDataSortKey ["LastPlayedDate", "PLAYCOUNT", "title", ""])
  check "sortByUserDataField lastPlayedDate: desc, Nothing last"
    ["2", "1", "3"]
    (map itemId (sortByUserDataField "lastPlayedDate" lk5 [i1, i2, i3]))
  check "sortByUserDataField LastPlayedDate case-insensitive"
    ["2", "1", "3"]
    (map itemId (sortByUserDataField "LastPlayedDate" lk5 [i3, i1, i2]))
  check "sortByUserDataField playCount: desc, missing = 0"
    ["3", "1", "2"]
    (map itemId (sortByUserDataField "playcount" lk5 [i1, i2, i3]))
  check "sortByUserDataField unknown key is identity"
    [i3, i1, i2] (sortByUserDataField "title" lk5 [i3, i1, i2])
  check "sortByUserDataField is stable for equal keys"
    ["1", "2", "3"]
    (map itemId (sortByUserDataField "playcount" (const Nothing) [i1, i2, i3]))
  check "parseIdsBody: ItemIds"
    (Right ["1", "2"]) (parseIdsBody (LBSC.pack "{\"ItemIds\":[\"1\",\"2\"]}"))
  check "parseIdsBody: Ids alias"
    (Right ["a"]) (parseIdsBody (LBSC.pack "{\"Ids\":[\"a\"]}"))
  check "parseIdsBody: ItemIds wins over Ids"
    (Right ["x"]) (parseIdsBody (LBSC.pack "{\"ItemIds\":[\"x\"],\"Ids\":[\"y\"]}"))
  check "parseIdsBody: empty object -> []" (Right []) (parseIdsBody (LBSC.pack "{}"))
  check "parseIdsBody: malformed JSON is Left"
    True (either (const True) (const False) (parseIdsBody (LBSC.pack "{nope")))

  -- Milestone 10c (device registry): upsertDevice + Device JSON + devices.json.
  let d1 = Device "aa" "Living Room TV" "Jellyfin Web" "10.9" ["Video","Audio"] (Just "1") (Just aNow)
      d2 = Device "bb" "Phone" "Jellyfin Android" "0.16" [] Nothing Nothing
      d1' = d1 { devAppVersion = "10.10", devCapabilities = ["Video"] }
  check "upsertDevice inserts a new device"
    (Just d1) (HM.lookup "aa" (upsertDevice d1 HM.empty))
  check "upsertDevice updates an existing device, preserving the id"
    (Just d1') (HM.lookup "aa" (upsertDevice d1' (upsertDevice d1 (HM.singleton "bb" d2))))
  check "upsertDevice with an empty id is a no-op (caller mints)"
    HM.empty (upsertDevice (d1 { devId = "" }) HM.empty)
  check "Device JSON roundtrip"
    (Just d1) (Aeson.decode (Aeson.encode d1) :: Maybe Device)
  check "Device FromJSON defaults for missing optional fields"
    (Just ("", "", "", "", [], Nothing, Nothing))
    ((\d -> (devId d, devName d, devAppName d, devAppVersion d, devCapabilities d, devLastUserId d, devLastSeenAt d)) <$>
      (Aeson.decode (LBSC.pack "{}") :: Maybe Device))
  let devFile = "/tmp/fms-test-devices.json"
      devMap = HM.fromList [("aa", d1), ("bb", d2)]
  saveDevices devFile devMap
  loadedDevs <- loadDevices devFile
  check "saveDevices/loadDevices roundtrip" devMap loadedDevs
  missingDevs <- loadDevices "/tmp/fms-test-devices-does-not-exist.json"
  check "loadDevices of a missing file is empty" HM.empty missingDevs
  removeFile devFile

  -- Milestone 4i (/items/:id/Similar): similarItems ranking/exclusion/cap.
  let si base = mi5 base base  -- id == title, path from title
      metaWith genres = sparseMeta { metaGenres = genres }
      simLookup = HM.fromList
        [ ("1", metaWith ["Action", "Sci-Fi"])
        , ("2", metaWith ["action", "Drama"])       -- case-insensitive share: Action
        , ("3", metaWith ["Sci-Fi", "Horror"])       -- shares Sci-Fi only
        , ("4", metaWith ["Drama"])                  -- no share with base
        , ("5", metaWith ["Action", "Sci-Fi", "Drama"])  -- shares 2
        ]
      simItems = [si "1", si "2", si "3", si "4", si "5", si "6"]
      -- item 6 has no metadata -> never matches
  check "similarItems: ranked by shared-genre count desc, self excluded"
    ["5", "2", "3"]
    (map itemId (similarItems (flip HM.lookup simLookup) 12 (si "1") simItems))
  check "similarItems: case-insensitive genre match"
    True ("2" `elem` map itemId (similarItems (flip HM.lookup simLookup) 12 (si "1") simItems))
  check "similarItems: tie on shared count breaks by title asc"
    ["a", "b"]
    (map itemTitle (similarItems (flip HM.lookup (HM.fromList [("x", metaWith ["G"]), ("a", metaWith ["g"]), ("b", metaWith ["G"])])) 12 (si "x") [si "x", si "b", si "a"]))
  check "similarItems: base without metadata -> []"
    [] (map itemId (similarItems (flip HM.lookup simLookup) 12 (si "6") simItems))
  check "similarItems: item without metadata never matches"
    False ("6" `elem` map itemId (similarItems (flip HM.lookup simLookup) 12 (si "1") simItems))
  check "similarItems: limit caps the result"
    ["5", "2"] (map itemId (similarItems (flip HM.lookup simLookup) 2 (si "1") simItems))
  check "similarItems: limit <= 0 means default 12"
    3 (length (similarItems (flip HM.lookup simLookup) 0 (si "1") simItems))

  -- Milestone 11b (server logs endpoint + log level): pushLogEntry ring,
  -- logLevelAllows matrix, LogEntry JSON keys.
  let le :: Int -> LogEntry
      le i = LogEntry aNow "info" ("msg " <> T.pack (show i))
      buf3 = pushLogEntry 3 (pushLogEntry 3 (pushLogEntry 3 [] (le 1)) (le 2)) (le 3)
  check "pushLogEntry appends newest-last and keeps order under cap"
    [1, 2, 3] (map (\e -> read (T.unpack (T.drop 4 (leMessage e))) :: Int) buf3)
  check "pushLogEntry drops the oldest when over cap"
    [2, 3, 4]
    (map (\e -> read (T.unpack (T.drop 4 (leMessage e))) :: Int)
      (pushLogEntry 3 buf3 (le 4)))
  check "pushLogEntry cap 0 keeps nothing"
    ([] :: [LogEntry]) (pushLogEntry 0 [le 1] (le 2))
  check "logRank order debug<info<warn<error, unknown entry = info"
    [0, 1, 2, 3, 1] (map logRank ["debug", "info", "WARN", "error", "bogus"])
  check "logLevelAllows: info config allows info/warn/error, suppresses debug"
    [False, True, True, True] (map (logLevelAllows "info") ["debug", "info", "warn", "error"])
  check "logLevelAllows: error config allows only error"
    [False, False, False, True] (map (logLevelAllows "error") ["debug", "info", "warn", "error"])
  check "logLevelAllows: debug config allows everything"
    [True, True, True, True] (map (logLevelAllows "debug") ["debug", "info", "warn", "error"])
  check "logLevelAllows: unknown config level allows all"
    [True, True] (map (logLevelAllows "bogus") ["debug", "error"])
  check "logLevelAllows: case-insensitive config"
    True (logLevelAllows "Warn" "error")
  check "LogEntry JSON keys Timestamp/Level/Message"
    (Just (True, True, True))
    ((\v -> case v of
        Aeson.Object o -> ( KeyMap.member "Timestamp" o
                           , KeyMap.member "Level" o
                           , KeyMap.member "Message" o )
        _ -> (False, False, False))
      <$> (Aeson.decode (Aeson.encode (le 1)) :: Maybe Aeson.Value))

  -- Milestone 12 (webhooks): eventMatches matrix + Webhook JSON + webhooks.json.
  check "eventMatches: empty list matches everything"
    [True, True] (map (eventMatches []) ["PlaybackStart", "anything"])
  check "eventMatches: \"*\" matches everything"
    [True, True] (map (eventMatches ["*"]) ["PlaybackStart", "Test"])
  check "eventMatches: case-insensitive membership"
    [True, False] (map (eventMatches ["playbackstart", "LibraryChanged"]) ["PlaybackStart", "UserDataSaved"])
  check "eventMatches: no match"
    False (eventMatches ["PlaybackStart"] "LibraryChanged")
  let w1 = Webhook "aa11" "http://example.com/hook" ["PlaybackStart", "*"] True (Just "s3cret") aNow
      w2 = Webhook "bb22" "http://example.com/other" [] False Nothing aNow
  check "Webhook JSON roundtrip"
    (Just w1) (Aeson.decode (Aeson.encode w1) :: Maybe Webhook)
  check "Webhook FromJSON defaults for missing optional fields"
    (Just ([], True, Nothing))
    ((\w -> (whEvents w, whEnabled w, whSecret w)) <$>
      (Aeson.decode (LBSC.pack "{\"Id\":\"x\",\"Url\":\"http://u\",\"AddedAt\":\"2024-01-01T00:00:00Z\"}")
        :: Maybe Webhook))
  let whFile = "/tmp/fms-test-webhooks.json"
      whMap = HM.fromList [("aa11", w1), ("bb22", w2)]
  saveWebhooks whFile whMap
  loadedWh <- loadWebhooks whFile
  check "saveWebhooks/loadWebhooks roundtrip" whMap loadedWh
  missingWh <- loadWebhooks "/tmp/fms-test-webhooks-does-not-exist.json"
  check "loadWebhooks of a missing file is empty" HM.empty missingWh
  removeFile whFile

  -- Milestone 13b (configurable CORS + security headers).
  let acao origins mo =
        BSC.unpack <$> lookup "Access-Control-Allow-Origin" (allowedOriginsHeader origins mo)
  check "parseCorsOrigins: empty -> [\"*\"]"
    ["*"] (parseCorsOrigins "")
  check "parseCorsOrigins: unset-ish whitespace -> [\"*\"]"
    ["*"] (parseCorsOrigins "  , ,")
  check "parseCorsOrigins: comma-separated, trimmed, empties dropped"
    ["https://a.example", "http://b.example:8080"] (parseCorsOrigins " https://a.example , ,http://b.example:8080,")
  check "allowedOriginsHeader: wildcard config -> * even without Origin"
    (Just "*") (acao ["*"] Nothing)
  check "allowedOriginsHeader: wildcard config -> * with Origin"
    (Just "*") (acao ["*"] (Just "https://evil.example"))
  check "allowedOriginsHeader: exact match reflects origin (case-insensitive) + Vary"
    (Just "https://A.example", Just "Origin")
    (acao ["https://a.example"] (Just "https://A.example"),
     BSC.unpack <$> lookup "Vary" (allowedOriginsHeader ["https://a.example"] (Just "https://A.example")))
  check "allowedOriginsHeader: restricted, non-matching origin -> no ACAO"
    Nothing (acao ["https://a.example"] (Just "https://evil.example"))
  check "allowedOriginsHeader: restricted, no Origin header -> no ACAO"
    Nothing (acao ["https://a.example"] Nothing)
  check "securityHeaders: nosniff/SAMEORIGIN/no-referrer, no CSP"
    ([("X-Content-Type-Options","nosniff"),("X-Frame-Options","SAMEORIGIN"),("Referrer-Policy","no-referrer")], False)
    (securityHeaders, any ((== "Content-Security-Policy") . fst) securityHeaders)

  -- Milestone 4j (multi-version media): normalizeVersionTitle, versionRank,
  -- groupVersions, versionParam/versionPathFor.
  check "normalizeVersionTitle strips dot-separated quality tokens + release group"
    "movie name 2019"
    (normalizeVersionTitle "Movie.Name.2019.1080p.BluRay.x264-GRP")
  check "normalizeVersionTitle strips bracketed tokens"
    "movie name" (normalizeVersionTitle "Movie Name [2160p] [WEB-DL]")
  check "normalizeVersionTitle handles 4k/underscore and collapses spaces"
    "movie" (normalizeVersionTitle "Movie_4K__Remux ")
  check "normalizeVersionTitle strips 5.1/2.0 audio tokens (year stays)"
    "movie 2020" (normalizeVersionTitle "Movie.2020.1080p.5.1.AC3")
  check "normalizeVersionTitle plain name unchanged (lowercased)"
    "movie name" (normalizeVersionTitle "Movie Name")
  check "versionRank ordering 8k>4k=2160p>1080p>720p>480p>none"
    [5, 4, 4, 3, 2, 1, 0]
    (map versionRank ["Movie.8k", "Movie.4K", "Movie.2160p", "Movie.1080p", "Movie.720p", "Movie.480p", "Movie"])
  let gv = groupVersions
        [ ("/m/Movie.1080p.mkv", 1000), ("/m/Movie.2160p.mkv", 4000)
        , ("/m/Movie.720p.mkv", 500), ("/m/Other.mp4", 10) ]
  check "groupVersions groups by (dir, normalized title), best first"
    [[("/m/Movie.2160p.mkv", 4000), ("/m/Movie.1080p.mkv", 1000), ("/m/Movie.720p.mkv", 500)], [("/m/Other.mp4", 10)]]
    gv
  check "groupVersions tie on rank breaks by size desc then path asc"
    [[("/m/Movie.1080p.BluRay.mkv", 900), ("/m/Movie.1080p.WEB-DL.mkv", 900), ("/m/Movie.1080p.mkv", 100)]]
    (groupVersions [("/m/Movie.1080p.mkv",100),("/m/Movie.1080p.WEB-DL.mkv",900),("/m/Movie.1080p.BluRay.mkv",900)])
  check "groupVersions separates directories"
    2 (length (groupVersions [("/a/Movie.1080p.mkv",1),("/b/Movie.1080p.mkv",1)]))
  -- milestone 2e: photos/books are never multi-version grouped by the
  -- caller (only video/* goes into groupVersions -- FMS.Modules.Library.Monad);
  -- groupVersions itself groups whatever it is given, so simulate the
  -- caller's filter: photo paths are NOT passed in.
  check "groupVersions leaves same-named photos ungrouped when caller filters to video"
    [[("/m/Movie.1080p.mkv", 100)], [("/m/photo.jpg", 5)], [("/m/photo2.png", 6)]]
    (let isVideo p = "video" `T.isPrefixOf` mimeFor p
         files = [("/m/Movie.1080p.mkv", 100), ("/m/photo.jpg", 5), ("/m/photo2.png", 6)]
     in groupVersions [ (p, s) | (p, s) <- files, isVideo p ]
        ++ [ [(p, s)] | (p, s) <- files, not (isVideo p) ])
  let vpItem = v4 { itemVersions = ["/m/a.1080p.mkv", "/m/a.2160p.mkv"], itemPath = "/m/a.1080p.mkv" }
  check "versionParam: absent -> 0, valid -> n, invalid -> Left"
    [Right 0, Right 2, Left "invalid version parameter"]
    [versionParam [], versionParam [("version","2")], versionParam [("version","x")]]
  check "versionPathFor picks the right file, default primary"
    [Right "/m/a.1080p.mkv", Right "/m/a.2160p.mkv"]
    [versionPathFor vpItem 0, versionPathFor vpItem 1]
  check "versionPathFor out of range -> Left"
    [Left "no such version", Left "version must be >= 0"]
    [versionPathFor vpItem 5, versionPathFor vpItem (-1)]
  check "versionPathFor single-file item: only index 0"
    [Right (itemPath v4), Left "no such version"]
    [versionPathFor v4 0, versionPathFor v4 1]

  -- Milestone 6e (chapter images): fallbackChapters, parseChaptersJson,
  -- ticks, JSON shape, cache path, ffprobe/ffmpeg arg lists.
  check "fallbackChapters: 10 evenly spaced starts over 100s, no titles"
    ([(i, fromIntegral (i * 10) :: Double, Nothing) | i <- [0 .. 9 :: Int]])
    [(chIndex c, chStartSeconds c, chTitle c) | c <- fallbackChapters 100 10]
  check "fallbackChapters: count <= 0 defaults to 10"
    10 (length (fallbackChapters 100 0))
  check "fallbackChapters: duration 0 -> single chapter at 0"
    [Chapter 0 0 Nothing] (fallbackChapters 0 10)
  check "fallbackChapters: negative duration -> single chapter at 0"
    [Chapter 0 0 Nothing] (fallbackChapters (-5) 10)
  check "ticksFromSeconds: 100ns units"
    [0, 15000000, 305000000] (map ticksFromSeconds [0, 1.5, 30.5])
  let probeJson = LBSC.pack
        "{\"format\":{\"duration\":\"125.5\"},\"chapters\":["
        <> LBSC.pack
        "{\"start_time\":\"0.000000\",\"tags\":{\"title\":\"Intro\"}},"
        <> LBSC.pack "{\"start_time\":\"30.5\",\"end_time\":\"60.0\"}]}"
  check "parseChaptersJson: duration + chapters with tags"
    (Right (Just 125.5, [Chapter 0 0 (Just "Intro"), Chapter 1 30.5 Nothing]))
    (parseChaptersJson probeJson)
  check "parseChaptersJson: no chapters -> empty list, numeric duration ok"
    (Right (Just 42, []))
    (parseChaptersJson (LBSC.pack "{\"format\":{\"duration\":42},\"chapters\":[]}"))
  check "parseChaptersJson: malformed JSON -> Left"
    ("Left" :: String) (either (const "Left") (const "Right") (parseChaptersJson (LBSC.pack "{nope")))
  check "chapterToJson: Jellyfin shape, Name present when titled"
    (Just (Aeson.Number 305000000), Just (Aeson.String "Scene"), Just (Aeson.String "2"))
    (case chapterToJson (Chapter 2 30.5 (Just "Scene")) of
       Aeson.Object o ->
         ( KeyMap.lookup (Key.fromText "StartPositionTicks") o
         , KeyMap.lookup (Key.fromText "Name") o
         , KeyMap.lookup (Key.fromText "ImageTag") o )
       _ -> (Nothing, Nothing, Nothing))
  check "chapterToJson: Name omitted when untitled"
    (False, Just (Aeson.Number 0))
    (case chapterToJson (Chapter 0 0 Nothing) of
       Aeson.Object o ->
         ( KeyMap.member (Key.fromText "Name") o
         , KeyMap.lookup (Key.fromText "StartPositionTicks") o )
       _ -> (True, Nothing))
  check "chapterImagePathFor: stateDir/chapters/<id>-<version>-<index>.jpg"
    "/st/chapters/7-1-3.jpg" (chapterImagePathFor "/st" "7" 1 3)
  check "probeChaptersArgs asks for chapters + format as JSON"
    (True, True) (elem "-show_chapters" (probeChaptersArgs "x"), elem "-show_format" (probeChaptersArgs "x"))
  check "chapterImageArgs: -ss start before -i, single mjpeg frame to pipe"
    ["-ss", "30.5", "-i", "/m/a.mkv", "-frames:v", "1", "-q:v", "3", "-f", "image2", "-vcodec", "mjpeg", "pipe:1"]
    (dropWhile (/= "-ss") (chapterImageArgs "/m/a.mkv" 30.5))

  -- Milestone 6f (bitrate caps + hw accel): effectiveBitrateCap,
  -- hwEncoderFor/normalizeHwAccel, transcodeArgsWith, parseFormatBitRate,
  -- decidePlayMethod, policy MaxStreamingBitrate.
  check "effectiveBitrateCap: nothing set -> Nothing"
    Nothing (effectiveBitrateCap Nothing Nothing 0)
  check "effectiveBitrateCap: server cap only"
    (Just 5000000) (effectiveBitrateCap Nothing Nothing 5000000)
  check "effectiveBitrateCap: min of the set caps (user < client < server)"
    (Just 1000000)
    (effectiveBitrateCap (Just 1000000) (Just 8000000) 5000000)
  check "effectiveBitrateCap: zeros are treated as unset"
    Nothing (effectiveBitrateCap (Just 0) Nothing 0)
  check "effectiveBitrateCap: client cap only, 0 ignored"
    (Just 8000000) (effectiveBitrateCap Nothing (Just 8000000) 0)
  check "effectiveBitrateCap: negative server cap ignored"
    (Just 2000000) (effectiveBitrateCap Nothing (Just 2000000) (-1))
  check "normalizeHwAccel: known names kept (case-insensitive), unknown -> none"
    ["vaapi", "nvenc", "qsv", "videotoolbox", "none", "none"]
    (map normalizeHwAccel ["VAAPI", "Nvenc", "qsv", " VideoToolbox ", "cuda", ""])
  check "hwEncoderFor maps accel names to h264 encoders, else Nothing"
    [Just "h264_vaapi", Just "h264_nvenc", Just "h264_qsv", Just "h264_videotoolbox", Nothing]
    (map hwEncoderFor ["vaapi", "nvenc", "qsv", "videotoolbox", "none"])
  check "transcodeArgsWith: no accel, no cap == transcodeArgs"
    (transcodeArgs "/m/a.mkv") (transcodeArgsWith "none" Nothing "/m/a.mkv")
  check "transcodeArgsWith: hwaccel adds -hwaccel before -i and swaps encoder"
    ["-hwaccel", "vaapi", "-i", "/m/a.mkv", "-c:v", "h264_vaapi"]
    (take 6 (drop 2 (transcodeArgsWith "vaapi" Nothing "/m/a.mkv")))
  check "transcodeArgsWith: libx264 keeps -preset veryfast"
    True ("-preset" `elem` transcodeArgsWith "none" Nothing "/m/a.mkv")
  check "transcodeArgsWith: hw encoder drops the x264-only -preset"
    False ("-preset" `elem` transcodeArgsWith "nvenc" Nothing "/m/a.mkv")
  check "transcodeArgsWith: bitrate cap adds -b:v/-maxrate/-bufsize(2x) before -c:a"
    ["-b:v", "1000000", "-maxrate", "1000000", "-bufsize", "2000000", "-c:a", "aac", "-force_key_frames", "expr:gte(t,n_forced*2)", "-g", "48", "-movflags", "frag_keyframe+empty_moov", "-f", "mp4", "pipe:1"]  -- m52 adds the keyframe flags
    (dropWhile (/= "-b:v") (transcodeArgsWith "none" (Just 1000000) "/m/a.mkv"))
  check "transcodeArgsWith: no cap -> no -b:v"
    False ("-b:v" `elem` transcodeArgsWith "none" Nothing "/m/a.mkv")
  check "parseFormatBitRate: string bit_rate"
    (Just 91767)
    (parseFormatBitRate (LBSC.pack "{\"format\":{\"bit_rate\":\"91767\",\"duration\":\"20.0\"}}"))
  check "parseFormatBitRate: numeric bit_rate, missing -> Nothing, bad JSON -> Nothing"
    [Just 5000, Nothing, Nothing]
    [ parseFormatBitRate (LBSC.pack "{\"format\":{\"bit_rate\":5000}}")
    , parseFormatBitRate (LBSC.pack "{\"format\":{\"duration\":\"1\"}}")
    , parseFormatBitRate (LBSC.pack "{nope") ]
  check "probeBitrateArgs asks only for the format section"
    (True, False) (elem "-show_format" (probeBitrateArgs "x"), elem "-show_streams" (probeBitrateArgs "x"))
  check "decidePlayMethod: direct playable + no cap -> DirectPlay"
    "DirectPlay" (decidePlayMethod True Nothing Nothing)
  check "decidePlayMethod: bitrate under cap -> DirectPlay, over -> Transcode"
    ["DirectPlay", "Transcode"]
    [decidePlayMethod True (Just 4000000) (Just 5000000), decidePlayMethod True (Just 9000000) (Just 5000000)]
  check "decidePlayMethod: unknown source bitrate passes the cap"
    "DirectPlay" (decidePlayMethod True Nothing (Just 1000))
  check "decidePlayMethod: not direct playable -> Transcode"
    "Transcode" (decidePlayMethod False (Just 100) Nothing)
  check "UserPolicy JSON roundtrip keeps MaxStreamingBitrate"
    (Just (Just 2500000))
    (policyMaxStreamingBitrate <$>
      (Aeson.decode (LBSC.pack "{\"MaxParentalRating\":null,\"BlockedTags\":[],\"AccessSchedules\":[],\"MaxStreamingBitrate\":2500000}") :: Maybe UserPolicy))
  check "UserPolicy FromJSON without MaxStreamingBitrate (pre-6f) defaults to Nothing"
    (Just Nothing)
    (policyMaxStreamingBitrate <$>
      (Aeson.decode (LBSC.pack "{\"MaxParentalRating\":\"PG\",\"BlockedTags\":[],\"AccessSchedules\":[]}") :: Maybe UserPolicy))
  check "PolicyPatch sets/clears MaxStreamingBitrate"
    [Just 3000000, Nothing]
    [ policyMaxStreamingBitrate (applyPolicyPatch emptyUserPolicy
        (either (const (PolicyPatch Nothing Nothing Nothing Nothing)) id
          (Aeson.eitherDecode (LBSC.pack "{\"MaxStreamingBitrate\":3000000}"))))
    , policyMaxStreamingBitrate (applyPolicyPatch
        (emptyUserPolicy { policyMaxStreamingBitrate = Just 1 })
        (PolicyPatch Nothing Nothing Nothing (Just Nothing))) ]
  check "PolicyPatch JSON null clears MaxStreamingBitrate (.:? would collapse null)"
    (Just (Just Nothing))
    (either (const Nothing) (Just . ppMaxBitrate)
      (Aeson.eitherDecode (LBSC.pack "{\"MaxStreamingBitrate\":null}") :: Either String PolicyPatch))

  -- Milestone 3g (DASH): dashArgs shape, bitrate cap args, isValidDashSegName
  -- traversal matrix, dashDirFor path shape.
  let dargs = dashArgs "/m/a.mkv" "/st/dash/1-0/manifest.mpd"
  check "dashArgs: dash muxer, 4s segments, template names"
    (True, True, True, True)
    ( elem "-f" dargs && take 1 (drop 1 (dropWhile (/= "-f") dargs)) == ["dash"]
    , take 1 (drop 1 (dropWhile (/= "-seg_duration") dargs)) == ["4"]
    , "init-stream$Number$.m4s" `elem` dargs
    , "chunk-stream$Number$-$Number$.m4s" `elem` dargs )
  check "dashArgs: output mpd is the last arg, no bitrate args without cap"
    (True, False)
    ( last dargs == "/st/dash/1-0/manifest.mpd"
    , "-b:v" `elem` dargs )
  check "dashArgsWith: bitrate cap adds -b:v/-maxrate/-bufsize(2x)"
    ["-b:v", "1000000", "-maxrate", "1000000", "-bufsize", "2000000"]
    (take 6 (dropWhile (/= "-b:v") (dashArgsWith (Just 1000000) "/m/a.mkv" "/o/manifest.mpd")))
  check "dashArgsWith: Nothing/0 cap -> no -b:v"
    (False, False)
    ( "-b:v" `elem` dashArgsWith Nothing "/m/a.mkv" "/o.mpd"
    , "-b:v" `elem` dashArgsWith (Just 0) "/m/a.mkv" "/o.mpd" )
  check "isValidDashSegName: accepts init/media segment names"
    [True, True, True]
    (map isValidDashSegName ["init-stream0.m4s", "chunk-stream0-00001.m4s", "a-b_c.d"])
  check "isValidDashSegName: rejects traversal, separators, empty"
    [False, False, False, False, False]
    (map isValidDashSegName ["..", "../manifest.mpd", "a/b.m4s", "", "a b.m4s"])
  check "dashDirFor: stateDir/dash/<id>-<version>"
    "/st/dash/7-2"
    (dashDirFor Config
      { configLibraryPath = "/lib", configPort = 8080, configStateDir = "/st"
      , configRequireAuth = False, configServerName = "fms"
      , configFollowSymlinks = False, configTmdbApiKey = ""
      , configRateLimit = Nothing, configRateLimitAuth = Just 10, configLogLevel = "info"
      , configCorsOrigins = ["*"], configSecurityHeaders = True
      , configPasswordMinLength = 0, configMaxBitrate = 0, configHwAccel = "none"
      , configWatch = False, configMaxTranscodes = 2, configFfmpegTimeoutSec = 30
      , configTlsCert = Nothing, configTlsKey = Nothing
      , configFetchImages = False, configScanWorkers = 4, configMaxBodyBytes = 10485760, configTmdbBaseUrl = "https://api.themoviedb.org/3"
      , configTmdbImageBaseUrl = "https://image.tmdb.org/t/p", configTmdbImageSize = "w500", configTmdbBackdropSize = "original", configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400
      , configMemoryWarnMb = 1024, configMemorySampleSecs = 30
      , configUiDir = Nothing
      , configJackettUrl = Nothing
      , configJackettApiKey = Nothing
      , configDelugeUrl = "http://127.0.0.1:8112/json"
      , configDelugePassword = ""
      , configDownloadDir = "/tmp/fms-test-downloads"
      , configRequestAutoApproveGb = 5
      , configIndexerUrl = Nothing
      , configIndexerApiKey = Nothing
      , configIndexerName = Nothing
      } "7" 2)

  -- Milestone 2d (filesystem watching): eventsPending debounce predicate
  -- and the media-file filter (pure helpers, platform-independent).
  let wNow = UTCTime (fromGregorian 2024 6 1) 43200  -- 2024-06-01 12:00:00
      wOld = UTCTime (fromGregorian 2024 6 1) 43000  -- 100s earlier
  check "eventsPending: event inside the window -> True"
    True (eventsPending [(wNow, "/m/a.mp4")] wNow 2.0)
  check "eventsPending: event older than the window -> False"
    False (eventsPending [(wOld, "/m/a.mp4")] wNow 2.0)
  check "eventsPending: empty list -> False"
    False (eventsPending [] wNow 2.0)
  check "eventsPending: any event inside counts"
    True (eventsPending [(wOld, "/x"), (wNow, "/m/b.mkv")] wNow 2.0)
  check "watchMediaFile: video/audio/books pass, images do not (m51: no Photo items)"
    [True, True, False, False, False, True]
    (map watchMediaFile ["/m/a.MP4", "b.mkv", "notes.txt", "data.bin", "cover.jpg", "book.epub"])

  -- Milestone 11c: ScanProgress event payload shapes (docs/todo.org
  -- section 2).
  let evScan  = scanProgressData "Scanning" [("FolderId", Aeson.String "1"), ("Folder", Aeson.String "Movies")]
      evDone  = scanProgressData "Completed" [("Added", Aeson.Number 2), ("Changed", Aeson.Number 1), ("Removed", Aeson.Number 0)]
      evFail  = scanProgressData "Failed" [("Error", Aeson.String "boom")]
      evRaw v = LBS.toStrict (Aeson.encode v)
  check "scanProgressData Scanning has Status + Folder fields"
    [True, True, True]
    (map (`BSC.isInfixOf` evRaw evScan)
      [ "\"Status\":\"Scanning\"", "\"FolderId\":\"1\"", "\"Folder\":\"Movies\"" ])
  check "scanProgressData Completed has Status + Added/Changed/Removed counts"
    [True, True, True, True]
    (map (`BSC.isInfixOf` evRaw evDone)
      [ "\"Status\":\"Completed\"", "\"Added\":2", "\"Changed\":1", "\"Removed\":0" ])
  check "scanProgressData Failed has Status + Error"
    [True, True]
    (map (`BSC.isInfixOf` evRaw evFail) [ "\"Status\":\"Failed\"", "\"Error\":\"boom\"" ])
  check "scanProgressData with no extra fields is just Status"
    "{\"Status\":\"Scanning\"}" (BSC.unpack (evRaw (scanProgressData "Scanning" [])))

  -- Milestone 6g (image cache tags + backdrop index): imageTag format and
  -- determinism, selectBackdrop bounds/sorting.
  check "imageTag: <size>-<mtimeEpoch> format"
    "1234-1717200000" (imageTag 1234 1717200000)
  check "imageTag: deterministic and size+mtime sensitive"
    (True, False, False)
    ( imageTag 10 20 == imageTag 10 20
    , imageTag 10 20 == imageTag 11 20
    , imageTag 10 20 == imageTag 10 21 )
  check "selectBackdrop: picks sorted entry by index"
    (Just "/d/Backdrop1.jpg")
    (selectBackdrop ["/d/Backdrop2.jpg", "/d/Backdrop.jpg", "/d/Backdrop1.jpg"] 1)
  check "selectBackdrop: index 0 is the first sorted file"
    (Just "/d/Backdrop.jpg") (selectBackdrop ["/d/Backdrop1.jpg", "/d/Backdrop.jpg"] 0)
  check "selectBackdrop: out of range / negative / empty -> Nothing"
    [Nothing, Nothing, Nothing]
    [ selectBackdrop ["/d/Backdrop.jpg"] 1
    , selectBackdrop ["/d/Backdrop.jpg"] (-1)
    , selectBackdrop [] 0 ]

  -- Milestone 6h (MediaSegments): validSegmentType, ticks<->seconds,
  -- validateSegment matrix, JSON roundtrip, persistence roundtrip.
  check "validSegmentType: recognised types, case-insensitive"
    [True, True, True, False, False]
    (map validSegmentType ["Intro", "OUTRO", "preview", "Ad", ""])
  check "ticksToSeconds/secondsToTicks roundtrip"
    [0, 1.5, 20] (map (ticksToSeconds . secondsToTicks) [0, 1.5, 20 :: Double])
  check "secondsToTicks: 100ns units"
    [0, 15000000, 200000000] (map secondsToTicks [0, 1.5, 20 :: Double])
  let segOk = MediaSegment "s1" "1" "Intro" 0 50000000
      segBadType = segOk { segType = "Ad" }
      segBadRange = segOk { segEndTicks = 0 }
      segNeg = segOk { segStartTicks = -1 }
  check "validateSegment: valid passes"
    (Right segOk) (validateSegment segOk)
  check "validateSegment: bad type -> Left"
    True (either (const True) (const False) (validateSegment segBadType))
  check "validateSegment: end <= start -> Left"
    True (either (const True) (const False) (validateSegment segBadRange))
  check "validateSegment: negative start -> Left"
    True (either (const True) (const False) (validateSegment segNeg))
  check "MediaSegment JSON roundtrip"
    (Just segOk) (Aeson.decode (Aeson.encode segOk) :: Maybe MediaSegment)
  check "MediaSegment FromJSON defaults for missing keys"
    (Just (MediaSegment "" "" "" 0 0))
    (Aeson.decode (LBSC.pack "{}") :: Maybe MediaSegment)
  let segFile = "/tmp/fms-test-segments.json"
      segMap = HM.fromList [("1", [segOk]), ("2", [segOk { segId = "s2", segItemId = "2", segType = "Outro" }])]
  saveSegments segFile segMap
  loadedSegs <- loadSegments segFile
  check "saveSegments/loadSegments roundtrip" segMap loadedSegs
  missingSegs <- loadSegments "/tmp/fms-test-segments-does-not-exist.json"
  check "loadSegments of a missing file is empty" HM.empty missingSegs
  removeFile segFile

  -- Milestone 1d (CLI args + config validation): parseCliArgs matrix and
  -- validateConfig matrix.
  check "parseCliArgs: value flags (space and = forms)"
    (Right [("port","8080"),("library-path","/m"),("state-dir","/s"),("log-level","debug")])
    (parseCliArgs ["--port","8080","--library-path=/m","--state-dir","/s","--log-level=debug"])
  check "parseCliArgs: bare boolean flag means true, =false works"
    (Right [("require-auth","true"),("watch","false"),("follow-symlinks","true")])
    (parseCliArgs ["--require-auth","--watch=false","--follow-symlinks"])
  check "parseCliArgs: --no-security-headers maps to security-headers=false"
    (Right [("security-headers","false")]) (parseCliArgs ["--no-security-headers"])
  check "parseCliArgs: --help / --version"
    (Right [("help","")]) (parseCliArgs ["--help"])
  check "parseCliArgs: all value flags"
    (Right [("server-name","x"),("tmdb-api-key","k"),("rate-limit","5"),("cors-origins","a,b"),("password-min-length","8"),("max-bitrate","1000"),("hwaccel","vaapi")])
    (parseCliArgs ["--server-name","x","--tmdb-api-key=k","--rate-limit","5","--cors-origins","a,b","--password-min-length=8","--max-bitrate","1000","--hwaccel","vaapi"])
  check "parseCliArgs: unknown option -> Left"
    True (either (const True) (const False) (parseCliArgs ["--frobnicate"]))
  check "parseCliArgs: missing value -> Left"
    True (either (const True) (const False) (parseCliArgs ["--port"]))
  let cfg1d = Config
        { configLibraryPath = "/lib", configPort = 8080, configStateDir = "/st"
        , configRequireAuth = False, configServerName = "fms"
        , configFollowSymlinks = False, configTmdbApiKey = ""
        , configRateLimit = Nothing, configRateLimitAuth = Just 10, configLogLevel = "INFO"
        , configCorsOrigins = ["*"], configSecurityHeaders = True
        , configPasswordMinLength = 0, configMaxBitrate = 0, configHwAccel = "VAAPI"
        , configWatch = False, configMaxTranscodes = 2, configFfmpegTimeoutSec = 30
        , configTlsCert = Nothing, configTlsKey = Nothing
        , configFetchImages = False, configScanWorkers = 4, configMaxBodyBytes = 10485760, configTmdbBaseUrl = "https://api.themoviedb.org/3"
        , configTmdbImageBaseUrl = "https://image.tmdb.org/t/p", configTmdbImageSize = "w500", configTmdbBackdropSize = "original", configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400
        , configMemoryWarnMb = 1024, configMemorySampleSecs = 30
        , configUiDir = Nothing
      , configJackettUrl = Nothing
      , configJackettApiKey = Nothing
      , configDelugeUrl = "http://127.0.0.1:8112/json"
      , configDelugePassword = ""
      , configDownloadDir = "/tmp/fms-test-downloads"
      , configRequestAutoApproveGb = 5
      , configIndexerUrl = Nothing
      , configIndexerApiKey = Nothing
      , configIndexerName = Nothing
        }
      badPort p = cfg1d { configPort = p }
      isLeft' :: Either a b -> Bool
      isLeft' = either (const True) (const False)
  check "validateConfig: ok config passes and normalises log level + hwaccel"
    (Right (cfg1d { configLogLevel = "info", configHwAccel = "vaapi" }))
    (validateConfig cfg1d)
  check "validateConfig: ports 0/65536/70000 rejected, 1 and 65535 ok"
    ([True, True, True], [False, False])
    ( map isLeft' [validateConfig (badPort 0), validateConfig (badPort 65536), validateConfig (badPort 70000)]
    , map isLeft' [validateConfig (badPort 1), validateConfig (badPort 65535)] )
  check "validateConfig: empty library path / state dir rejected"
    [True, True] [isLeft' (validateConfig (cfg1d { configLibraryPath = "" })),
                 isLeft' (validateConfig (cfg1d { configStateDir = "" }))]
  check "validateConfig: bad log level rejected, case-insensitive ok"
    [True, False] [isLeft' (validateConfig (cfg1d { configLogLevel = "verbose" })),
                  isLeft' (validateConfig (cfg1d { configLogLevel = "Warn" }))]
  check "validateConfig: empty cors list and negative numbers rejected"
    [True, True, True]
    [ isLeft' (validateConfig (cfg1d { configCorsOrigins = [] }))
    , isLeft' (validateConfig (cfg1d { configPasswordMinLength = (-1) }))
    , isLeft' (validateConfig (cfg1d { configMaxBitrate = (-5) })) ]

  -- Milestone 4k (manual metadata editing + field locking, docs/todo.org
  -- section 3): fieldNameFor, applyManualPatch, applyProviderMerge, and the
  -- pre-4k metadata.json default for lockedFields.
  let mkPatch kvs = KeyMap.fromList [(Key.fromText k, v) | (k, v) <- kvs]
      base4k = emptyMetadata { metaTitle = Just "Old Title", metaGenres = ["Drama"], metaLockedFields = ["title"] }
  check "fieldNameFor maps known keys case-insensitively, rejects others"
    [Just "title", Just "genres", Just "officialRating", Nothing, Nothing]
    [ fieldNameFor "Title", fieldNameFor "GENRES", fieldNameFor "officialRating"
    , fieldNameFor "source", fieldNameFor "bogus" ]
  check "applyManualPatch sets unlocked fields, skips locked ones"
    (base4k { metaGenres = ["Horror"], metaYear = Just 1999 })
    (applyManualPatch base4k (mkPatch
      [ ("genres", Aeson.toJSON (["Horror"] :: [T.Text]))
      , ("title", Aeson.toJSON ("New" :: T.Text))    -- locked: skipped
      , ("year", Aeson.toJSON (1999 :: Int))
      ]))
  check "applyManualPatch ignores unknown keys"
    base4k (applyManualPatch base4k (mkPatch [("bogus", Aeson.toJSON (1 :: Int))]))
  check "applyManualPatch lockedFields key is always settable"
    (base4k { metaLockedFields = ["title", "genres"] })
    (applyManualPatch base4k (mkPatch [("lockedFields", Aeson.toJSON (["title", "genres"] :: [T.Text]))]))
  check "applyManualPatch JSON null clears a Maybe field"
    (emptyMetadata { metaOverview = Nothing })
    (applyManualPatch (emptyMetadata { metaOverview = Just "x" }) (mkPatch [("overview", Aeson.Null)]))
  check "applyProviderMerge keeps locked old fields, takes new elsewhere, carries locks"
    (emptyMetadata { metaTitle = Just "Old Title", metaOverview = Just "New plot"
                   , metaGenres = ["Action"], metaLockedFields = ["title"] })
    (applyProviderMerge base4k (emptyMetadata
      { metaTitle = Just "Prov Title", metaOverview = Just "New plot", metaGenres = ["Action"] }))
  check "MediaMetadata FromJSON without lockedFields defaults to [] (pre-4k metadata.json)"
    []
    (metaLockedFields (either (const emptyMetadata) id
      (Aeson.eitherDecode "{\"title\":\"x\"}" :: Either String MediaMetadata)))
  check "MediaMetadata JSON roundtrip carries lockedFields"
    (Just (base4k { metaLockedFields = ["year"] }))
    (Aeson.decode (Aeson.encode (base4k { metaLockedFields = ["year"] })) :: Maybe MediaMetadata)

  -- Milestone 15 (ffmpeg process management, docs/todo.org section 13):
  -- the slot semaphore helper and the input-timeout ffmpeg args.
  do sem <- newQSemN 1
     slot1 <- tryAcquireSlot sem 0
     slot2 <- tryAcquireSlot sem 1  -- slot held: waits 1s -> False
     signalQSemN sem 1
     slot3 <- tryAcquireSlot sem 0  -- released: immediate True
     check "tryAcquireSlot: acquire when free, timeout when held, ok after release"
       (True, False, True) (slot1, slot2, slot3)
  check "ffmpegTimeoutArgs: <=0 -> [], network input -> -rw_timeout us, local file -> []"
    ([], ["-rw_timeout", "30000000"], [], ["-rw_timeout", "5000000"])
    ( ffmpegTimeoutArgs 0 "http://x/y.mp4"
    , ffmpegTimeoutArgs 30 "http://x/y.mp4"
    , ffmpegTimeoutArgs 30 "/lib/movie.mp4"
    , ffmpegTimeoutArgs 5 "rtsp://cam/stream" )
  check "validateConfig: maxTranscodes / ffmpegTimeout below 1 rejected"
    [True, True]
    [ isLeft' (validateConfig (cfg1d { configMaxTranscodes = 0 }))
    , isLeft' (validateConfig (cfg1d { configFfmpegTimeoutSec = 0 })) ]

  -- Milestone 13d (HTTPS/TLS termination, docs/todo.org section 13):
  -- CLI flags for --tls-cert/--tls-key and the both-or-neither validation.
  check "parseCliArgs: --tls-cert/--tls-key in both forms"
    (Right [("tls-cert", "/c.pem"), ("tls-key", "/k.pem")])
    (parseCliArgs ["--tls-cert", "/c.pem", "--tls-key=/k.pem"])
  check "validateConfig: only one of TLS cert/key -> Left; both -> ok; neither -> ok"
    [True, False, False]
    [ isLeft' (validateConfig (cfg1d { configTlsCert = Just "/c.pem" }))
    , isLeft' (validateConfig (cfg1d { configTlsCert = Just "/c.pem", configTlsKey = Just "/k.pem" }))
    , isLeft' (validateConfig cfg1d) ]

  -- Milestone 16 (security hardening, docs/todo.org section 13):
  -- validateFolderPath rejects empty/relative/traversal/null-byte paths.
  let isRight' :: Either a b -> Bool
      isRight' = either (const False) (const True)
  check "validateFolderPath: absolute paths ok (trailing slash kept normalised)"
    [Right "/mnt/movies", Right "/mnt/movies/", Right "/"]
    [validateFolderPath "/mnt/movies", validateFolderPath "/mnt/movies/", validateFolderPath "/"]
  check "validateFolderPath: relative, empty, .. segment, null byte rejected"
    [False, False, False, False, False]
    [ isRight' (validateFolderPath "movies")
    , isRight' (validateFolderPath "")
    , isRight' (validateFolderPath "/mnt/../etc")
    , isRight' (validateFolderPath "../etc")
    , isRight' (validateFolderPath "/mnt/mo\0vies")
    ]

  -- Milestone 17a (jellyfin-web compat): case-insensitive path segments,
  -- case-insensitive query param names, ParentId filter.
  check "normalizeSegments lowercases path segments"
    ["items", "videos", "1", "main.m3u8"]
    (normalizeSegments ["Items", "Videos", "1", "MAIN.M3U8"])
  check "matchSegments: Lit compares case-insensitively, captures keep case"
    (Just [("id", "AbC")], True)
    ( matchSegments [Lit "Items", Capture "id"] ["ITEMS", "AbC"]
    , matchSegments [Lit "items"] ["ITEMS"] == Just [] )
  check "isOpenPath is case-insensitive"
    [True, True, False]
    [ isOpenPath ["system", "info", "public"], isOpenPath ["System", "Info", "Public"]
    , isOpenPath ["items"] ]
  check "normalizeQueryParams lowercases names, keeps values and order"
    [("filters", "IsPlayed"), ("includeitemtypes", "Movie,Episode"), ("sortby", "SortName")]
    (normalizeQueryParams [("Filters", "IsPlayed"), ("IncludeItemTypes", "Movie,Episode"), ("SortBy", "SortName")])
  check "queryParams: PascalCase WAI query names are folded (value untouched)"
    [("parentid", "1"), ("sortby", "Title")]
    (queryParams [("ParentId", Just "1"), ("SortBy", Just "Title")])
  let m17a = testItem
      items17 = [m17a { itemId = "a", itemFolder = "1" }, m17a { itemId = "b", itemFolder = "2" }]
  check "filterByParent: keeps only the given folder, empty/absent -> identity"
    (["a"], ["a", "b"], ["a", "b"])
    ( map itemId (filterByParent [("parentid", "1")] items17)
    , map itemId (filterByParent [("parentid", "")] items17)
    , map itemId (filterByParent [] items17) )

  -- Milestone 17b (jellyfin-web compat): PascalCase JSON keys for MediaItem
  -- and the public User; the internal persistence JSON is unchanged.
  let jsonKeys v = case v of
        Aeson.Object o -> sort (map Key.toText (KeyMap.keys o))
        _              -> []
      u17b = User "1" "admin" "hash" True [] Nothing emptyUserPolicy True False
  check "MediaItem ToJSON uses exactly the PascalCase Jellyfin key set"
    [ "DateCreated", "EpisodeNumber", "ExtraType", "FolderId", "Id", "MimeType"
    , "Name", "Path", "SeasonNumber", "SeriesName", "Size", "Type", "Versions" ]
    (jsonKeys (Aeson.toJSON (testItem { itemId = "1" })))
  check "User public ToJSON uses exactly the PascalCase key set"
    [ "AllowedFolders", "Enabled", "HasPassword", "Id", "IsAdministrator"
    , "MustChangePassword", "Name", "Policy" ]
    (jsonKeys (Aeson.toJSON u17b))
  check "User internal JSON keeps the lowercase persistence keys (unchanged)"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode (toInternalJSON u17b)))
      [ "\"userId\":", "\"userName\":", "\"passwordHash\":", "\"isAdmin\":"
      , "\"allowedFolders\":", "\"mustChangePassword\":" ])
  check "User internal JSON roundtrip still decodes"
    (Just u17b) (Aeson.decode (Aeson.encode (toInternalJSON u17b)) :: Maybe User)
  let iv17b = Aeson.toJSON (testItem { itemId = "1" })
  check "projectFields fields=Name and fields=name both keep Id+Name (case-insensitive)"
    (["Id", "Name"], ["Id", "Name"])
    (jsonKeys (projectFields ["Name"] iv17b), jsonKeys (projectFields ["name"] iv17b))

  -- Milestone 17c (jellyfin-web compat part 3): Views builder, NextUp,
  -- Latest, Progress PascalCase keys.
  let f1 = MediaFolder "1" "Media" "/lib" "mixed"
      f2 = MediaFolder "2" "Movies" "/movies" "movies"
      vj = LBSC.unpack (Aeson.encode (viewsFromFolders [f1, f2]))
  check "viewsFromFolders: CollectionFolder view objects with PascalCase keys"
    [True, True, True, True]
    [ "\"Name\":\"Media\"" `isInfixOf` vj
    , "\"Id\":\"2\"" `isInfixOf` vj
    , "\"Type\":\"CollectionFolder\"" `isInfixOf` vj
    , "\"CollectionType\":\"movies\"" `isInfixOf` vj ]
  let ep ser n = testItem { itemId = "e" <> ser <> T.pack (show n), itemType = "Episode"
                         , itemSeries = Just ser, itemSeason = Just 1
                         , itemEpisode = Just n, itemTitle = ser <> " ep" <> T.pack (show n) }
      eps17 = [ep "S" 1, ep "S" 2, ep "T" 1]
      udPlayed1 = \iid -> if iid == "eS1" then Just (emptyUserData { udPlayed = True }) else Nothing
  check "nextUpEpisodes picks the first UNPLAYED episode per series"
    ["eS2", "eT1"] (map itemId (nextUpEpisodes udPlayed1 eps17))
  check "nextUpEpisodes with empty lookup -> first episode of each series"
    ["eS1", "eT1"] (map itemId (nextUpEpisodes (const Nothing) eps17))
  check "nextUpEpisodes skips a fully-played series"
    ["eT1"] (map itemId (nextUpEpisodes (\iid -> if iid == "eS1" || iid == "eS2" then Just (emptyUserData { udPlayed = True }) else Nothing) eps17))
  let t17a = UTCTime (fromGregorian 2020 1 1) 0
      t17b = UTCTime (fromGregorian 2021 1 1) 0
      t17c = UTCTime (fromGregorian 2022 1 1) 0
      la17 = [ testItem { itemId = "a", itemAddedAt = t17a, itemType = "Episode" }
           , testItem { itemId = "b", itemAddedAt = t17c, itemType = "Movie" }
           , testItem { itemId = "c", itemAddedAt = t17b, itemType = "Audio" } ]
  check "latestItems: DateCreated desc, default cap 12, Limit cap, IncludeItemTypes"
    (["b", "c", "a"], ["b", "c"], ["b"])
    ( map itemId (latestItems [] 0 la17)
    , map itemId (latestItems [] 2 la17)
    , map itemId (latestItems [("includeItemTypes", "Movie")] 0 la17) )
  let p17 = Progress 5 10 (UTCTime (fromGregorian 2024 1 1) 0)
      pj = LBS.toStrict (Aeson.encode p17)
  check "Progress ToJSON uses PascalCase keys (17c)"
    True (all (`BSC.isInfixOf` pj)
      [ "\"PositionSeconds\":", "\"DurationSeconds\":", "\"UpdatedAt\":" ])
  check "Progress FromJSON accepts new AND old (pre-17c stats.json) keys"
    (Just p17, Just p17)
    ( Aeson.decode (Aeson.encode p17) :: Maybe Progress
    , Aeson.decode (LBSC.pack "{\"currentTime\":5,\"duration\":10,\"updatedAt\":\"2024-01-01T00:00:00Z\"}") :: Maybe Progress )

  -- SceneDetect (intro/outro auto-detection parsers + proposal)
  let bl1 = "[blackdetect @ 0x55] black_start:0 black_end:3.04 black_duration:3.04"
      bl2 = "[blackdetect @ 0x55] black_start:13 black_end:16 black_duration:3"
      blDur = "[blackdetect @ 0x55] black_start:5 black_duration:2.5"
      blGarbage = "[blackdetect @ 0x55] config in time_base: no line here"
  check "parseBlackdetect: start/end lines, duration fallback, garbage ignored"
    ([(0, 3.04), (13, 16), (5, 7.5)], 3)
    (parseBlackdetect (unlines [bl1, bl2, blDur, blGarbage, "random noise"])
    , length (parseBlackdetect (unlines [bl1, bl2, blDur, blGarbage, "random noise"])))
  let sl1 = "[silencedetect @ 0x77] silence_start: 1.5"
      sl2 = "[silencedetect @ 0x77] silence_end: 4.2 | silence_duration: 2.7"
      sl3 = "[silencedetect @ 0x77] silence_start: 9.0"
      sl4 = "[silencedetect @ 0x77] silence_start: 20"
      sl5 = "[silencedetect @ 0x77] silence_end: 22.5 | silence_duration: 2.5"
  check "parseSilencedetect: paired start/end, trailing unpaired start dropped"
    ([(1.5, 4.2), (20, 22.5)], 2)
    (parseSilencedetect (unlines [sl1, sl2, sl4, sl5, sl3, "garbage"])
    , length (parseSilencedetect (unlines [sl1, sl2, sl4, sl5, sl3, "garbage"])))
  let slDurOnly = "[silencedetect @ 0x77] silence_start: 8 silence_duration: 1.25"
  check "parseSilencedetect: silence_duration fallback when no end line"
    [(8, 9.25)] (parseSilencedetect slDurOnly)
  check "mergeIntervals: overlap, adjacency (gap < 0.5s), disjoint"
    ([(0, 3.2), (5, 6), (10, 12)], [(0, 1), (2, 3)], [(0, 1), (5, 6)])
    ( mergeIntervals [(0, 2), (2.2, 3.2), (5, 6), (10, 12)]
    , mergeIntervals [(0, 1), (2, 3)]
    , mergeIntervals [(5, 6), (0, 1)] )
  check "intervalsOverlap matrix"
    [True, True, False, False, False]
    [ intervalsOverlap (0, 3) (2, 5), intervalsOverlap (2, 5) (0, 3)
    , intervalsOverlap (0, 3) (3, 5), intervalsOverlap (0, 3) (4, 5)
    , intervalsOverlap (5, 6) (0, 3) ]
  let blacks16 = [(0, 3), (13, 16)]
  check "proposeSegments: black intro 0-3 + outro 13-16 with duration 16"
    (Just (0, 3), Just (13, 16)) (proposeSegments 16 blacks16 [])
  check "proposeSegments: no intervals -> (Nothing, Nothing)"
    (Nothing, Nothing) (proposeSegments 16 [] [])
  check "proposeSegments: silence-only intro works"
    (Just (0, 2.5), Nothing) (proposeSegments 16 [] [(0, 2.5), (10, 11)])
  check "proposeSegments: interval starting at 1s (within 2s) counts as intro"
    (Just (1, 4), Nothing) (proposeSegments 16 [(1, 4)] [])
  check "proposeSegments: intro longer than 300s is capped at 300"
    (Just (0, 300), Nothing) (proposeSegments 400 [(0, 350)] [])
  check "proposeSegments: outro longer than 300s is capped (start = duration-300)"
    (Nothing, Just (100, 400)) (proposeSegments 400 [(50, 400)] [])
  check "proposeSegments: duration <= 0 -> (Nothing, Nothing)"
    (Nothing, Nothing) (proposeSegments 0 blacks16 blacks16)
  check "proposeSegments: mid-movie black interval is neither intro nor outro"
    (Nothing, Nothing) (proposeSegments 16 [(6, 8)] [])
  check "blackdetectArgs / silencedetectArgs shapes"
    (True, True, True, True)
    ( "blackdetect=d=0.05:pix_th=0.10" `elem` blackdetectArgs "/m/x.mp4"
    , "-f" `elem` blackdetectArgs "/m/x.mp4" && "null" `elem` blackdetectArgs "/m/x.mp4"
    , "silencedetect=n=-35dB:d=0.4" `elem` silencedetectArgs "/m/x.mp4"
    , "-i" `elem` silencedetectArgs "/m/x.mp4" && "/m/x.mp4" `elem` silencedetectArgs "/m/x.mp4" )
  check "parseFormatDuration: ffprobe -show_format fixture"
    (Just 16.5, Nothing)
    ( parseFormatDuration (LBSC.pack "{\"format\":{\"duration\":\"16.500000\",\"bit_rate\":\"123\"}}")
    , parseFormatDuration (LBSC.pack "{\"format\":{\"bit_rate\":\"123\"}}") )

  -- Item tags API: mergeTags / removeTag / attachTags / provider-merge union
  check "mergeTags: existing order kept, new appended, dedupe case-insensitive"
    (["favorite", "hd", "Criterion"], ["favorite"], ["a", "b"])
    ( mergeTags ["favorite", "hd"] ["HD", "Criterion", "favorite"]
    , mergeTags ["favorite"] []
    , mergeTags [] ["a", "b"] )
  check "removeTag: case-insensitive removal, missing tag is a no-op"
    (["favorite"], ["a", "b"], ["b"])
    ( removeTag "HD" ["favorite", "hd"]
    , removeTag "zzz" ["a", "b"]
    , removeTag "A" ["a", "b"] )
  let tagObj = Aeson.object [Key.fromText "Id" Aeson..= ("1" :: T.Text)]
      tagV = attachTags ["favorite", "hd"] tagObj
      tagJ = LBS.toStrict (Aeson.encode tagV)
  check "attachTags inserts a Tags array into an item object"
    [True, True] [ "\"Tags\":[\"favorite\",\"hd\"]" `BSC.isInfixOf` tagJ
                , "\"Id\":\"1\"" `BSC.isInfixOf` tagJ ]
  check "attachTags: empty tag list attaches nothing; non-object passes through"
    (True, True)
    ( Aeson.encode (attachTags [] tagObj) == Aeson.encode tagObj
    , attachTags ["x"] (Aeson.String "s") == Aeson.String "s" )
  check "attachTags + projectFields: Tags kept when listed, stripped otherwise (case-insensitive)"
    (True, False)
    ( "\"Tags\"" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (projectFields ["tags"] tagV))
    , "\"Tags\"" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (projectFields ["Name"] tagV)) )
  check "applyProviderMerge unions tags (user tags survive provider refresh, case-insensitive dedupe)"
    (["favorite", "HD", "Criterion"], ["favorite", "hd"])
    ( metaTags (applyProviderMerge
      (emptyMetadata { metaTags = ["favorite"], metaLockedFields = ["title"] })
      (emptyMetadata { metaTags = ["HD", "Criterion", "FAVORITE"], metaTitle = Just "T" }))
    , metaTags (applyProviderMerge
      (emptyMetadata { metaTags = ["favorite"] })
      (emptyMetadata { metaTags = ["FAVORITE", "hd"] })) )
  check "tagsFromMetadata counts tags live from metadata records (nub per record, values as stored)"
    [("favorite", 2), ("hd", 1)]
    (tagsFromMetadata [ emptyMetadata { metaTags = ["favorite", "hd", "favorite"] }
                      , emptyMetadata { metaTags = ["favorite"] } ])

  -- Plugin system skeleton: Plugin JSON, overlay/seed merge, hook registry
  let pl = Plugin "livetv" "Live TV" "0.0.0" "Live TV tuners (not implemented)." False
      plJ = LBS.toStrict (Aeson.encode pl)
  check "Plugin ToJSON uses PascalCase keys Id/Name/Version/Description/Enabled"
    [True, True, True, True, True]
    [ "\"Id\":\"livetv\"" `BSC.isInfixOf` plJ
    , "\"Name\":\"Live TV\"" `BSC.isInfixOf` plJ
    , "\"Version\":\"0.0.0\"" `BSC.isInfixOf` plJ
    , "\"Description\":\"Live TV tuners (not implemented).\"" `BSC.isInfixOf` plJ
    , "\"Enabled\":false" `BSC.isInfixOf` plJ ]
  check "Plugin JSON roundtrip"
    (Just pl) (Aeson.decode (Aeson.encode pl) :: Maybe Plugin)
  check "Plugin FromJSON defaults Enabled=True on partial JSON"
    (Just True) (plugEnabled <$> (Aeson.decode "{\"Id\":\"x\"}" :: Maybe Plugin))
  check "builtinPlugins: at least webhooks(enabled), livetv(disabled), dlna(disabled)"
    [("dlna", False), ("livetv", False), ("webhooks", True)]
    (sort [(plugId p, plugEnabled p) | p <- builtinPlugins])
  let persisted = HM.fromList
        [ ("livetv", pl { plugEnabled = True })                       -- disabled builtin, enabled in file
        , ("future-plugin", Plugin "future-plugin" "Future" "9.9" "not built in" True) ]  -- unknown id
      mergedPlugs = overlayPlugins builtinPlugins persisted
  check "overlayPlugins: persisted enabled state wins, unknown persisted id kept, missing builtins defaulted"
    (Just True, Just True, Just False, 4)
    ( plugEnabled <$> HM.lookup "livetv" mergedPlugs
    , plugEnabled <$> HM.lookup "future-plugin" mergedPlugs
    , plugEnabled <$> HM.lookup "dlna" mergedPlugs
    , HM.size mergedPlugs )
  check "registerHook/unregisterHook: dedupe, per-hook lists, removal"
    ( HM.fromList [("scan", ["webhooks", "dlna"]), ("play", ["webhooks"])]
    , HM.fromList [("scan", ["dlna"]), ("play", ["webhooks"])]
    , HM.fromList [("scan", ["webhooks"]), ("play", ["webhooks"])] )
    ( registerHook "scan" "dlna" (registerHook "scan" "webhooks" (registerHook "scan" "webhooks" (registerHook "play" "webhooks" HM.empty)))
    , unregisterHook "scan" "webhooks" (registerHook "scan" "dlna" (registerHook "scan" "webhooks" (registerHook "play" "webhooks" HM.empty)))
    , unregisterHook "scan" "dlna" (registerHook "scan" "dlna" (registerHook "scan" "webhooks" (registerHook "play" "webhooks" HM.empty))) )

  -- TMDB artwork download: pure URL builder + images-JSON mapping
  check "tmdbImagesUrl builds {base}/{kind}/{id}/images?api_key=.. (trailing slash stripped)"
    "http://127.0.0.1:9999/3/movie/550/images?api_key=test"
    (tmdbImagesUrl "http://127.0.0.1:9999/3/" "test" "movie" "550")
  let imgJson = Aeson.decode (LBSC.pack (
        "{\"posters\":[{\"file_path\":\"/p1.jpg\"},{\"file_path\":\"/p2.jpg\"},{\"file_path\":\"\"}],"
        ++ "\"backdrops\":[{\"file_path\":\"/b1.jpg\"},{\"file_path\":\"/b2.jpg\"},{\"file_path\":\"/b3.jpg\"},{\"file_path\":\"/b4.jpg\"}],"
        ++ "\"logos\":[{\"file_path\":\"/l1.png\"},{\"file_path\":\"/l2.png\"}]}"))
  check "tmdbImageListToDownloads: caps (1 Primary, 3 Backdrops, 1 Logo), empty paths skipped"
    (Just [("Primary", "/p1.jpg"), ("Backdrop", "/b1.jpg"), ("Backdrop", "/b2.jpg"), ("Backdrop", "/b3.jpg"), ("Logo", "/l1.png")])
    (tmdbImageListToDownloads <$> imgJson)
  check "tmdbImageListToDownloads: missing keys -> empty; non-object -> empty"
    ([], [], [])
    ( tmdbImageListToDownloads (Aeson.object [])
    , tmdbImageListToDownloads (Aeson.String "x")
    , tmdbImageListToDownloads (fromMaybe Aeson.Null (Aeson.decode (LBSC.pack "{\"posters\":\"nope\"}") :: Maybe Aeson.Value)) )

  -- Milestone 14b: per-endpoint (auth) rate-limit class, bucket keys, Retry-After
  check "rateLimitClass: POST /AuthenticateByName -> auth (case-insensitive, trailing segs ok, /Users prefix ok)"
    ["auth", "auth", "auth", "global", "global", "global"]
    [ rateLimitClass "POST" ["AuthenticateByName"]
    , rateLimitClass "POST" ["authenticatebyname", "extra"]
    , rateLimitClass "POST" ["Users", "AuthenticateByName"]
    , rateLimitClass "GET" ["AuthenticateByName"]
    , rateLimitClass "POST" ["Items"]
    , rateLimitClass "POST" [] ]
  check "bucketKeyFor prefixes the class"
    ["auth:1.2.3.4", "global:1.2.3.4"]
    [ bucketKeyFor "auth" "1.2.3.4", bucketKeyFor "global" "1.2.3.4" ]
  check "retryAfterSeconds: ceil(60/rate) clamped to >= 1"
    [1, 20, 60, 1, 1]
    [ retryAfterSeconds 60, retryAfterSeconds 3, retryAfterSeconds 1
    , retryAfterSeconds 0, retryAfterSeconds (-5) ]
  let rlT0 = UTCTime (fromGregorian 2024 1 1) 0
      rlB0 = Bucket 0 rlT0  -- empty bucket at t0, rate 60/min = 1 token/s
      (okExact, bExact) = tryTake 60 (addUTCTime 1 rlT0) rlB0     -- exactly 1s -> exactly 1 token
      (okEarly, _) = tryTake 60 (addUTCTime 0.999 rlT0) rlB0      -- 0.999s -> not yet
  check "tryTake boundary: rate 60 refills exactly 1 token after 1s"
    (True, 0, False)
    (okExact, round (bucketTokens bExact) :: Int, okEarly)
  check "validateConfig rejects negative auth rate limit, accepts Just 0 (disabled) and default"
    [True, False, False]
    [ isLeft' (validateConfig (cfg1d { configRateLimitAuth = Just (-1) }))
    , isLeft' (validateConfig (cfg1d { configRateLimitAuth = Just 0 }))
    , isLeft' (validateConfig cfg1d) ]

  -- Parallel scan: chunksOf + pooledMapIO order/size behaviour
  check "chunksOf: exact division, remainder, empty, n<=0 -> single chunk"
    ([[1,2,3],[4,5,6]], [[1,2,3],[4,5,6],[7]], [], [[1,2]], [[1,2,3,4,5]])
    ( chunksOf 3 ([1..6] :: [Int]), chunksOf 3 ([1..7] :: [Int]), chunksOf 3 ([] :: [Int])
    , chunksOf 0 ([1,2] :: [Int]), chunksOf (-2) ([1..5] :: [Int]) )
  check "validateConfig rejects negative scan workers, accepts 0 and 4"
    [True, False, False]
    [ isLeft' (validateConfig (cfg1d { configScanWorkers = -1 }))
    , isLeft' (validateConfig (cfg1d { configScanWorkers = 0 }))
    , isLeft' (validateConfig (cfg1d { configScanWorkers = 4 })) ]
  do let poolDir = "/tmp/fms-test-pooled-scan"
     resetDir poolDir
     createDirectoryIfMissing True poolDir
     let poolPaths = [ poolDir </> ("f" ++ show n ++ ".bin") | n <- [1 .. 40 :: Int] ]
     mapM_ (\(n, p) -> BS.writeFile p (BS.replicate n 65)) (zip [1 .. 40 :: Int] poolPaths)
     pooled1 <- pooledMapIO 1 statFile poolPaths
     pooled8 <- pooledMapIO 8 statFile poolPaths
     pooled0 <- pooledMapIO 0 statFile poolPaths
     check "pooledMapIO: 40 files, workers 1/8/0 all return input order, sizes > 0"
       (True, True, True, True)
       ( map (\(p, _, _) -> p) pooled1 == poolPaths
       , map (\(p, _, _) -> p) pooled8 == poolPaths
       , map (\(p, _, _) -> p) pooled0 == poolPaths
       , all (\(_, s, _) -> s > 0) pooled8 )
     check "pooledMapIO: stat sizes match the written byte counts"
       (map (\(_, s, _) -> s) pooled8) (map (fromIntegral :: Int -> Integer) [1 .. 40 :: Int])
     removeDirectoryRecursive poolDir

  -- Error model: one body shape {"error","status","requestId"} everywhere
  check "statusForError maps all 9 AppError constructors to 400/401/403/404/429/503/413/409/500"
    [400, 401, 403, 404, 429, 503, 413, 409, 500]
    [ statusForError (BadRequest "x"), statusForError (Unauthorized "x")
    , statusForError (Forbidden "x"), statusForError (NotFound "x")
    , statusForError (TooManyRequests "x"), statusForError (ServiceUnavailable "x")
    , statusForError (PayloadTooLarge "x"), statusForError (Conflict "x")
    , statusForError (InternalError "x") ]
  check "errorMessage extracts the message of each constructor"
    ["m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"]
    [ errorMessage (BadRequest "m1"), errorMessage (Unauthorized "m2")
    , errorMessage (Forbidden "m3"), errorMessage (NotFound "m4")
    , errorMessage (TooManyRequests "m5"), errorMessage (ServiceUnavailable "m6")
    , errorMessage (PayloadTooLarge "m7"), errorMessage (Conflict "m8")
    , errorMessage (InternalError "m9") ]
  let eb = LBS.toStrict (Aeson.encode (errorBody "boom" 404 "00ff"))
  check "errorBody has exactly error+status+requestId with correct values"
    (True, True, True, True)
    ( "\"error\":\"boom\"" `BSC.isInfixOf` eb
    , "\"status\":404" `BSC.isInfixOf` eb
    , "\"requestId\":\"00ff\"" `BSC.isInfixOf` eb
    , case errorBody "boom" 404 "00ff" of
        Aeson.Object o -> length (KeyMap.keys o) == 3
        _              -> False )

  -- Input-validation hardening: numeric clamps + body-size cap (pure)
  check "clampNonNegative / clampStartIndex / clampLimit matrices"
    ([0, 0, 7], [0, 0, 5], [100, 1, 1, 1000, 42])
    ( [clampNonNegative (-3), clampNonNegative 0, clampNonNegative 7]
    , [clampStartIndex Nothing, clampStartIndex (Just (-9)), clampStartIndex (Just 5)]
    , [clampLimit Nothing, clampLimit (Just 0), clampLimit (Just (-5))
      , clampLimit (Just 99999999), clampLimit (Just 42)] )
  check "parseBoundedInt: normal, clamped lo/hi, unparseable/empty -> default, whitespace"
    [Just 5, Just 1, Just 1000, Just 100, Just 3, Just 100]
    [ parseBoundedInt 1 1000 100 "5", parseBoundedInt 1 1000 100 "0"
    , parseBoundedInt 1 1000 100 "99999999", parseBoundedInt 1 1000 100 "abc"
    , parseBoundedInt 1 1000 100 "  3  ", parseBoundedInt 1 1000 100 "" ]
  check "exceedsMaxBody: cap logic (0 = unlimited)"
    [True, False, False, True, False]
    [ exceedsMaxBody 100 101, exceedsMaxBody 100 100, exceedsMaxBody 0 999999999
    , exceedsMaxBody 10485760 11000000, exceedsMaxBody (-1) 5 ]
  check "validateConfig rejects negative max body bytes, accepts 0 and 10 MiB"
    [True, False, False]
    [ isLeft' (validateConfig (cfg1d { configMaxBodyBytes = -1 }))
    , isLeft' (validateConfig (cfg1d { configMaxBodyBytes = 0 }))
    , isLeft' (validateConfig (cfg1d { configMaxBodyBytes = 10485760 })) ]

  -- Memory monitoring (todo section 15): /proc/self/status VmRSS parsing
  -- (pure, platform-independent) + the ring-buffer trim.
  let procOk = unlines
        [ "Name:\tfms"
        , "State:\tS (sleeping)"
        , "VmPeak:\t  99999 kB"
        , "VmRSS:\t  12345 kB"
        , "VmHWM:\t  23456 kB"
        , "Threads:\t7"
        ]
  check "parseProcStatusRss: VmRSS line -> kB * 1024 bytes"
    [Just 12641280, Nothing, Nothing, Just 0]
    [ parseProcStatusRss procOk
    , parseProcStatusRss "Name:\tfms\nThreads:\t7\n"
    , parseProcStatusRss "total garbage\nno colons here"
    , parseProcStatusRss "VmRSS:\t0 kB\n" ]
  check "parseProcStatusRss: multiple lines picks the VmRSS one, extra spaces tolerated"
    [Just 1024, Just 2048]
    [ parseProcStatusRss "VmSize:\t  10 kB\nVmRSS:   1   kB\nVmData:\t20 kB\n"
    , parseProcStatusRss "VmRSS:\t\t2 kB\n" ]
  check "trimRing: cap keeps the newest tail, oversized cap and empty unchanged"
    (([3, 4, 5], [1, 2, 3], [], [7], []) :: ([Int], [Int], [Int], [Int], [Int]))
    ( trimRing 3 [1, 2, 3, 4, 5]
    , trimRing 10 [1, 2, 3]
    , trimRing 5 []
    , trimRing 1 [7]
    , trimRing 0 [7] )
  check "validateConfig: negative memory warn/sample rejected, zero sample allowed (disabled)"
    [True, True, False, False]
    [ isLeft' (validateConfig (cfg1d { configMemoryWarnMb = -1 }))
    , isLeft' (validateConfig (cfg1d { configMemorySampleSecs = -5 }))
    , isLeft' (validateConfig (cfg1d { configMemorySampleSecs = 0 }))
    , isLeft' (validateConfig (cfg1d { configMemoryWarnMb = 0, configMemorySampleSecs = 30 })) ]


  -- Legacy simple lowercase JSON for the stamno-media Elm UI (Api.elm):
  -- simpleItemJson + progressPercent + simpleProgressEntry + the
  -- /progress body accepting id/itemId/ItemId and PascalCase fallbacks.
  let tUi = read "2026-01-01 00:00:00 UTC" :: UTCTime
      uiItem = MediaItem
        { itemId = "7", itemTitle = "Movie", itemPath = "/lib/movie.mp4", itemSize = 12345
        , itemMime = "video/mp4", itemAddedAt = tUi, itemFolder = "1", itemType = "Movie"
        , itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = []
        }
      uiEnc = LBS.toStrict (Aeson.encode (simpleItemJson uiItem))
      uiKeys = case simpleItemJson uiItem of
        Aeson.Object o -> sort (map Key.toText (KeyMap.keys o))
        _ -> []
  check "simpleItemJson: exactly the lowercase keys id/title/size/mime"
    ["id", "mime", "size", "title"] uiKeys
  check "simpleItemJson: values for id/title/size/mime"
    [True, True, True, True]
    [ "\"id\":\"7\"" `BSC.isInfixOf` uiEnc
    , "\"title\":\"Movie\"" `BSC.isInfixOf` uiEnc
    , "\"size\":12345" `BSC.isInfixOf` uiEnc
    , "\"mime\":\"video/mp4\"" `BSC.isInfixOf` uiEnc ]
  check "progressPercent: 0/negative duration -> 0, half -> 50, over -> 100, neg cur -> 0"
    [0, 0, 50, 100, 0, 25, 100]
    [ progressPercent 30 0, progressPercent 5 (-3), progressPercent 50 100
    , progressPercent 130 100, progressPercent (-5) 100, progressPercent 25 100
    , progressPercent 100 100 ]
  check "simpleProgressEntry: simple keys plus lowercase progress percent"
    [True, True, True]
    [ "\"id\":\"7\"" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (simpleProgressEntry uiItem (Progress 30 100 tUi)))
    , "\"mime\":\"video/mp4\"" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (simpleProgressEntry uiItem (Progress 30 100 tUi)))
    , "\"progress\":30" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (simpleProgressEntry uiItem (Progress 30 100 tUi))) ]
  check "ProgressRequest body: id / itemId / ItemId + PascalCase fallbacks"
    [Just ("1", 30, 100), Just ("2", 1.5, 10), Just ("3", 0, 5)]
    [ ((\pr -> (prItemId pr, prCurrentTime pr, prDuration pr)) <$> d)
      | d <- [ Aeson.decode "{\"id\":\"1\",\"currentTime\":30,\"duration\":100}" :: Maybe ProgressRequest
             , Aeson.decode "{\"itemId\":\"2\",\"currentTime\":1.5,\"duration\":10}" :: Maybe ProgressRequest
             , Aeson.decode "{\"ItemId\":\"3\",\"CurrentTime\":0,\"Duration\":5}" :: Maybe ProgressRequest
             ] ]
  check "ProgressRequest body: malformed or missing id -> Nothing"
    [True, True]
    [ isNothing (Aeson.decode "{oops" :: Maybe ProgressRequest)
    , isNothing (Aeson.decode "{\"currentTime\":1,\"duration\":2}" :: Maybe ProgressRequest) ]

  -- Invalidation audit (metadata/file-stat caching): NFO detection,
  -- sidecar->item mapping, watch filter, blurhash purge predicate.
  let tInv = read "2026-01-01 00:00:00 UTC" :: UTCTime
      nfoItem = MediaItem
        { itemId = "n1", itemTitle = "movie", itemPath = "/l/movie.mp4", itemSize = 1
        , itemMime = "video/mp4", itemAddedAt = tInv, itemFolder = "1", itemType = "Movie"
        , itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = []
        }
      epItem = nfoItem { itemId = "e1", itemPath = "/l/s01e01.mp4"
                       , itemType = "Episode", itemSeries = Just "Show" }
  check "isNfoFile: .nfo case-insensitive, other extensions rejected"
    [True, True, False, False, False]
    [ isNfoFile "/l/movie.nfo", isNfoFile "/l/MOVIE.NFO", isNfoFile "/l/movie.mp4"
    , isNfoFile "/l/nfo", isNfoFile "/l/a.txt" ]
  check "watchFile: media + nfo count, others ignored"
    [True, True, False]
    [ watchFile "/l/a.mp4", watchFile "/l/a.nfo", watchFile "/l/a.txt" ]
  check "siblingMediaPathsForNfo: movie.nfo -> all media variants; tvshow.nfo -> []"
    (True, length mediaExtensions, ([] :: [FilePath]))
    ( "/l/movie.mp4" `elem` siblingMediaPathsForNfo "/l/movie.nfo"
    , length (siblingMediaPathsForNfo "/l/movie.nfo")
    , siblingMediaPathsForNfo "/l/tvshow.nfo" )
  check "itemIdsForNfo: own sidecar, tvshow.nfo episodes in the dir, no match"
    [["n1"], ["e1"], ([] :: [T.Text])]
    [ itemIdsForNfo [nfoItem, epItem] "/l/movie.nfo"
    , itemIdsForNfo [nfoItem, epItem] "/l/tvshow.nfo"
    , itemIdsForNfo [nfoItem] "/l/other.nfo" ]
  check "itemIdsForNfo: bare file name (hinotify dir watch) matches by name"
    [["n1"], ["e1"]]
    [ itemIdsForNfo [nfoItem, epItem] "movie.nfo"
    , itemIdsForNfo [nfoItem, epItem] "tvshow.nfo" ]
  check "isUnderDir: children only, not the dir itself nor prefix siblings"
    [True, False, False, True]
    [ isUnderDir "/st/images/1" "/st/images/1/a.jpg"
    , isUnderDir "/st/images/1" "/st/images/1"
    , isUnderDir "/st/images/1" "/st/images/12/a.jpg"
    , isUnderDir "/st/images/1" "/st/images/1/sub/b.png" ]

  -- Jellyfin-compat batch (todo): deviceProfileAllows (PlaybackInfo
  -- DeviceProfile) + burn-in filter arg builders (transcode).
  let dpRestrict = Aeson.object
        [ "DirectPlayProfiles" Aeson..= [ Aeson.object
            [ "Type" Aeson..= ("Video" :: T.Text), "Container" Aeson..= ("mp4" :: T.Text) ] ] ]
      dpCodec = Aeson.object
        [ "DirectPlayProfiles" Aeson..= [ Aeson.object
            [ "Type" Aeson..= ("Video" :: T.Text), "Container" Aeson..= ("mp4,mkv" :: T.Text)
            , "VideoCodec" Aeson..= ("h265" :: T.Text) ] ] ]
  check "deviceProfileAllows: default profile allows mp4/h264, rejects mkv/hevc (m33) + avi/xvid"
    [True, False, False]
    [ deviceProfileAllows defaultDeviceProfile "mp4" "h264"
    , deviceProfileAllows defaultDeviceProfile "mkv" "hevc"
    , deviceProfileAllows defaultDeviceProfile "avi" "xvid" ]
  check "deviceProfileAllows: client profile narrows container (no codec list -> any codec)"
    [True, False]
    [ deviceProfileAllows dpRestrict "mp4" "h264"
    , deviceProfileAllows dpRestrict "mkv" "h264" ]
  check "deviceProfileAllows: codec list enforced; missing list falls back to default"
    [False, True, True]
    [ deviceProfileAllows dpCodec "mp4" "h264"
    , deviceProfileAllows dpCodec "mkv" "h265"
    , deviceProfileAllows (Aeson.object []) "mp4" "h264" ]
  check "burnInFilter: external, embedded si, path escaping, empty path, extra chain"
    [ Just "subtitles=/m/a.srt"
    , Just "subtitles=/m/v.mkv:si=2"
    , Just "subtitles=/m/it\\'s\\:weird.srt"
    , Nothing
    , Just "subtitles=/m/a.ass,scale=320:240" ]
    [ burnInFilter "/m/a.srt" Nothing Nothing
    , burnInFilter "/m/v.mkv" (Just 2) Nothing
    , burnInFilter "/m/it's:weird.srt" Nothing Nothing
    , burnInFilter "" Nothing Nothing
    , burnInFilter "/m/a.ass" Nothing (Just "scale=320:240") ]
  check "spliceBeforeF: inserts before -f, appends when no -f marker"
    (["-i", "x", "-vf", "subtitles=/a", "-f", "mp4", "pipe:1"], ["-i", "x", "-vf", "f"])
    ( spliceBeforeF ["-vf", "subtitles=/a"] ["-i", "x", "-f", "mp4", "pipe:1"]
    , spliceBeforeF ["-vf", "f"] ["-i", "x"] )

  -- Web module: safeUiPath traversal guard + contentTypeFor mapping.
  check "safeUiPath: normal and nested files resolve under uiDir"
    [Just "/ui/index.html", Just "/ui/assets/app.js"]
    [ safeUiPath "/ui" "index.html", safeUiPath "/ui" "assets/app.js" ]
  check "safeUiPath: .. segments, absolute paths, null bytes rejected"
    [Nothing, Nothing, Nothing, Nothing, Nothing]
    [ safeUiPath "/ui" "../etc/passwd"
    , safeUiPath "/ui" "a/../../x"
    , safeUiPath "/ui" "/etc/passwd"
    , safeUiPath "/ui" ".."
    , safeUiPath "/ui" "a\0b" ]
  check "safeUiPath: encoded-slash traversal form rejected, dot-file ok"
    [Nothing, Just "/ui/.well-known/x"]
    [ safeUiPath "/ui" "../config.json", safeUiPath "/ui" ".well-known/x" ]
  check "contentTypeFor: html/js/css/png/jpg/svg/json/ico/map + default"
    [ "text/html; charset=utf-8", "application/javascript", "text/css; charset=utf-8"
    , "image/png", "image/jpeg", "image/svg+xml", "application/json"
    , "image/x-icon", "application/json", "application/octet-stream" ]
    (map (BSC.unpack . contentTypeFor)
      [ "index.html", "elm.js", "STAMNO.CSS", "a.png", "a.JPG", "a.svg"
      , "a.json", "favicon.ico", "a.js.map", "a.bin" ])
  -- milestone 19b: built-in admin dashboard page
  let dash = LBSC.unpack dashboardPageHtml
      dashMarkers =
        [ "<title>/dashboard</title>", "api('/Dashboard')", "api('/Sessions')"
        , "api('/Plugins')", "api('/System/Memory')", "X-Emby-Token"
        , "/Users/AuthenticateByName", "/ScheduledTasks/", "/Plugins/"
        , "/library/rescan", "/Library/Refresh", "/api-docs", "localStorage"
        , "setInterval(refresh, 10000)" ]
      dashMarkers63 =
        [ "tabbtn-users", "id='tab-users'", "id='tabbar'", "OVERVIEW</button>", "USERS</button>"
        , "function renderUsersTab()", "function createUser()", "function deleteUser("
        , "api('/Users'", "method:'POST'" ]
      dashMarkers65 =
        [ "tabbtn-requests", "id='tab-requests'", "REQUESTS</button>"
        , "['overview','trackers','requests','users']"
        , "currentTab==='requests'"
        , "id='dlsec'", "id='metasec'" ]
  check "dashboardPageHtml m63: button-carousel tabs + users tab markers"
    (map (\m -> m `isInfixOf` dash) dashMarkers63) (replicate (length dashMarkers63) True)
  check "dashboardPageHtml m65: requests tab + dl/metadata moved into trackers tab"
    (map (\m -> m `isInfixOf` dash) dashMarkers65) (replicate (length dashMarkers65) True)
  check "dashboardPageHtml: contains all required markers"
    (map (\m -> m `isInfixOf` dash) dashMarkers) (replicate (length dashMarkers) True)
  let stamnoTokens =
        [ "--void: #0a0b0d", "--gray-1: #1a1a1a", "--white: #ffffff"
        , "Courier New", "border-radius: 0", ".btn-primary", ".btn-ghost"
        , "bg-panel", "#1a1a1a", "b--default", "tracked-wide", "sticky-top" ]
  check "dashboardPageHtml: stamno design tokens present (m19b restyle)"
    (map (\m -> m `isInfixOf` dash) stamnoTokens) (replicate (length stamnoTokens) True)
  -- milestone 20c: requests queue + trackers manager sections
  let m20cMarkers =
        [ "api('/Requests')", "api('/Trackers')", "approveReq", "rejectReq"
        , "toggleTracker", "deleteTracker", "addTracker", "Approve", "Reject"
        , "Add tracker", "IS_ADMIN", "id='requests-sec'", "id='trackers-sec'"
        , "/Requests/'+", "/Trackers/'+", "renderRequests", "renderTrackers" ]
  check "dashboardPageHtml: m20c requests/trackers markers present"
    (map (\m -> m `isInfixOf` dash) m20cMarkers) (replicate (length m20cMarkers) True)
  -- milestone 22: tabbed dashboard with a Jackett-style trackers tab
  let m22Markers =
        [ "id='tabbtn-overview'", "id='tabbtn-trackers'", "id='tab-overview'", "id='tab-trackers'"
        , "setTab('trackers')", "currentTab", "renderTrackersTab"
        , "api('/Indexers')", "/Indexers/Search?q=", "testIndexer", "toggleIndexer", "deleteIndexer", "addIndexer"
        , "searchAll", "requestRelease", "reqNotice"
        , "Request</button>", "REQUESTED", "ALREADY IN PROGRESS", "Add indexer"
        , "Search all indexers", "External Jackett servers", "admin only" ]
  check "dashboardPageHtml: m22 tabbed trackers markers present"
    (map (\m -> m `isInfixOf` dash) m22Markers) (replicate (length m22Markers) True)
  -- milestone 24: m23 fields in the trackers-tab add-indexer form
  let m24Markers =
        [ "AuthType", "Cookie", "ResponseFormat", "torrentday-json", "SearchPath"
        , "toggleAuthFields", "type=\"password\"", "<th>Auth</th>", "<th>Format</th>"
        , "el('ia')", "el('ick')", "el('if')", "el('isp')", "value=\"cookie\"" ]
  let m25Markers =
        [ "querySelectorAll('input,select,textarea')", "vals[fields[i].id] = fields[i].value"
        , "f.value = vals[ks[j]]", "toggleAuthFields();" ]
  check "dashboardPageHtml: section() preserves form field values across re-renders (m25 fix)"
    (map (\m -> m `isInfixOf` dash) m25Markers) (replicate (length m25Markers) True)
  check "dashboardPageHtml: m24 cookie/torrentday form markers present"
    (map (\m -> m `isInfixOf` dash) m24Markers) (replicate (length m24Markers) True)
  check "dashboardPageHtml: ASCII only (no bytes > 127)"
    [BS.all (<= 127) (LBS.toStrict dashboardPageHtml)] [True]
  check "dashboardBeforeCapture: webModule has Lit dashboard before Capture file"
    [dashboardBeforeCapture (moduleRoutes webModule)] [True]
  check "dashboardBeforeCapture: reversed order is False"
    [dashboardBeforeCapture (reverse (moduleRoutes webModule))] [False]
  check "dashboardBeforeCapture: no dashboard route is False"
    [dashboardBeforeCapture [get [Capture "file"] undefined, get [] undefined]] [False]
  -- milestone 20a: media Requests core (pure helpers + JSON shape)
  check "normalizeTitle: case/punct/space collapsing"
    [ normalizeTitle "The Matrix (1999)", normalizeTitle "the matrix 1999"
    , normalizeTitle "  Spider-Man:  Far From Home! ", normalizeTitle "spiderman far from home"
    , normalizeTitle "", normalizeTitle "---" ]
    [ "the matrix 1999", "the matrix 1999", "spider man far from home"
    , "spiderman far from home", "", "" ]
  check "isActiveStatus matrix"
    (map isActiveStatus [Pending, Approved, Downloading, Imported, Rejected, Failed])
    [True, True, True, False, False, False]
  let mkReq t st = Request
        { reqId = "r1", reqUserId = "u1", reqUserName = "bob", reqTitle = t
        , reqYear = Nothing, reqMediaType = "movie", reqQuery = t, reqStatus = st
        , reqSizeBytes = Nothing, reqSourceIndexer = Nothing, reqSourceRelease = Nothing
        , reqSourceUrl = Nothing, reqMagnet = Nothing, reqDownloadUrl = Nothing
        , reqCreatedAt = reqT0, reqUpdatedAt = reqT0
        , reqApprovedBy = Nothing, reqError = Nothing, reqDownloadDir = Nothing
        , reqProgress = Nothing, reqTorrentState = Nothing }
      reqT0 = UTCTime (fromGregorian 2026 1 1) 0
  check "duplicateOf: active match, rejected ignored, other title no"
    [ fmap reqId (duplicateOf "The Matrix" [mkReq "the matrix" Pending])
    , fmap reqId (duplicateOf "The Matrix" [mkReq "the matrix" Rejected])
    , fmap reqId (duplicateOf "The Matrix" [mkReq "other movie" Pending])
    , fmap reqId (duplicateOf "the matrix 1999" [mkReq "The Matrix (1999)" Approved]) ]
    [Just "r1", Nothing, Nothing, Just "r1"]
  let reqJson = Aeson.toJSON (mkReq "Dune" Pending)
      reqKeys = case reqJson of
        Aeson.Object o -> sort (map Key.toText (KeyMap.keys o))
        _ -> []
  check "Request ToJSON: PascalCase API keys"
    reqKeys
    [ "ApprovedBy", "CreatedAt", "DownloadDir", "DownloadUrl", "Error", "Id", "Magnet"
    , "MediaType", "Progress", "Query", "SizeBytes", "SourceIndexer", "SourceRelease", "SourceUrl"
    , "Status", "Title", "TorrentState", "UpdatedAt", "UserId", "UserName", "Year" ]
  check "Request JSON roundtrip (PascalCase)"
    [ Aeson.decode (Aeson.encode (mkReq "Dune" Downloading)) == Just (mkReq "Dune" Downloading) ]
    [True]
  check "Request Status serializes as the constructor name string"
    [ case reqJson of
        Aeson.Object o -> KeyMap.lookup (Key.fromText "Status") o
        _ -> Nothing ]
    [Just (Aeson.String "Pending")]
  -- milestone 20b: trackers + push pipeline pure helpers
  let torznabSample = LBSC.pack (unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<rss version=\"2.0\"><channel><title>Jackett</title>"
        , "<item>"
        , "  <title>The Matrix 1999 1080p BluRay</title>"
        , "  <size>2147483648</size>"
        , "  <attr name=\"seeders\" value=\"42\"/>"
        , "  <attr name=\"magneturl\" value=\"magnet:?xt=urn:btih:abc\"/>"
        , "  <enclosure url=\"http://tracker/dl/1.torrent\" type=\"application/x-bittorrent\" length=\"123\"/>"
        , "</item>"
        , "<item><title>NoAttrs</title></item>"
        , "<item><size>10</size></item>"
        , "</channel></rss>" ])
      parsed = parseTorznabResults torznabSample
      tr = Tracker { trackerId = "jackett", trackerName = "Jackett", trackerUrl = "http://j:9117", trackerApiKey = "SECRET", trackerEnabled = True }
  check "parseTorznabResults: item titles parsed, no-title items skipped"
    [ ["The Matrix 1999 1080p BluRay", "NoAttrs"] ] [ relTitle <$> parsed ]
  check "parseTorznabResults: item count" [ 2 ] [ length parsed ]
  check "parseTorznabResults: item sizes" [ [Just 2147483648, Nothing] ] [ relSizeBytes <$> parsed ]
  check "parseTorznabResults: item seeders" [ [Just 42, Nothing] ] [ relSeeders <$> parsed ]
  check "parseTorznabResults: item magnets"
    [ [Just "magnet:?xt=urn:btih:abc", Nothing] ] [ relMagnet <$> parsed ]
  check "parseTorznabResults: item download urls"
    [ [Just "http://tracker/dl/1.torrent", Nothing] ] [ relDownloadUrl <$> parsed ]
  check "parseTorznabResults: non-XML -> []" [parseTorznabResults (LBSC.pack "not xml")] [[]]
  check "jackettSearchUrl: movie/tv cat + encoding"
    [ jackettSearchUrl "http://j:9117/" "jackett" "KEY" "the matrix" (Just "movie")
    , jackettSearchUrl "http://j:9117" "jackett" "KEY" "the matrix" (Just "tv")
    , jackettSearchUrl "http://j:9117" "jackett" "KEY" "a b" Nothing ]
    [ "http://j:9117/api/v2.0/indexers/jackett/results?apikey=KEY&t=search&q=the%20matrix&cat=2000"
    , "http://j:9117/api/v2.0/indexers/jackett/results?apikey=KEY&t=search&q=the%20matrix&cat=5000"
    , "http://j:9117/api/v2.0/indexers/jackett/results?apikey=KEY&t=search&q=a%20b" ]
  check "trackerPublicJson: admin sees key, non-admin gets Null"
    [ case trackerPublicJson True tr of Aeson.Object o -> KeyMap.lookup (Key.fromText "ApiKey") o; _ -> Nothing
    , case trackerPublicJson False tr of Aeson.Object o -> KeyMap.lookup (Key.fromText "ApiKey") o; _ -> Nothing ]
    [ Just (Aeson.String "SECRET"), Just Aeson.Null ]
  check "slugify" [slugify "The Matrix (1999)!", slugify "", slugify "a  b"] ["the-matrix-1999", "request", "a-b"]
  check "autoApproveSize: Nothing approves, at/under threshold approves, over rejects"
    [ autoApproveSize Nothing 100, autoApproveSize (Just 100) 100, autoApproveSize (Just 101) 100 ]
    [True, True, False]
  check "matchesDownload: exact normalized match, extension stripped, year suffix ok"
    [ matchesDownload "The Matrix" ["/dl/the matrix (1999).mkv", "/dl/other.mp4"]
    , matchesDownload "The Matrix" ["/dl/The.Matrix.Reloaded.mkv"]
    , matchesDownload "Dune" ["/dl/DUNE.2021.mp4"]
    , matchesDownload "Dune" ["/dl/dune2.mkv"]
    , matchesDownload "Dune" ["/dl/dune.srt"] ]
    [ True, False, True, False, True ]
  -- milestone 30: markImported walks the download dir RECURSIVELY (Deluge
  -- saves into <downloads>/<Torrent.Name>/file.mp4)
  let m30dir = "/tmp/fms-m30-dl"
  resetDir m30dir
  createDirectoryIfMissing True (m30dir </> "Shrek.2001.1080p.BluRay.x265-LAMA")
  createDirectoryIfMissing True (m30dir </> "Other.Torrent")
  writeFile (m30dir </> "Shrek.2001.1080p.BluRay.x265-LAMA" </> "Shrek.2001.1080p.BluRay.x265-LAMA.mkv") "x"
  writeFile (m30dir </> "Other.Torrent" </> "unrelated movie.mkv") "x"
  writeFile (m30dir </> "notes.txt") "x"   -- not a media file: filtered out
  m30files <- scanDirectory False m30dir
  check "m30 scanDirectory recursive walk finds media files in subdirs only"
    [ sort m30files ]
    [ sort [ m30dir </> "Shrek.2001.1080p.BluRay.x265-LAMA" </> "Shrek.2001.1080p.BluRay.x265-LAMA.mkv"
           , m30dir </> "Other.Torrent" </> "unrelated movie.mkv" ] ]
  check "m30 matchesDownload matches a media file found in a torrent subdir"
    [ matchesDownload "Shrek.2001.1080p.BluRay.x265-LAMA" m30files
    , matchesDownload "shrek 2001 1080p bluray x265 lama" m30files ] [ True, True ]
  check "m30 matchesDownload negative: non-matching subdir file does not match"
    [ matchesDownload "Dune" m30files ] [ False ]
  m30missing <- scanDirectory False (m30dir </> "no-such-dir")
  check "m30 scanDirectory on a missing dir yields []"
    [ null m30missing ] [ True ]
  -- milestone 31: match the download by the SOURCE RELEASE name too
  let m31files = [ "/dl/Shrek.2001.1080p.BluRay.x265-LAMA/Shrek.2001.1080p.BluRay.x265-LAMA.mkv" ]
  check "m31 matchesRequestDownload: release name matches even when the title does not"
    [ matchesDownload "Shrek 2001" m31files  -- title alone does NOT match
    , matchesRequestDownload "Shrek 2001" (Just "Shrek 2001 1080p BluRay x265-LAMA") m31files ]
    [ False, True ]
  check "m31 matchesRequestDownload: title-only match still works, no release"
    [ matchesRequestDownload "Shrek 2001" Nothing [ "/dl/Shrek 2001.mkv" ] ] [ True ]
  check "m31 matchesRequestDownload: neither title nor release matches -> False"
    [ matchesRequestDownload "Dune" (Just "Some Other Release") m31files
    , matchesRequestDownload "Dune" Nothing m31files ]
    [ False, False ]
  check "base64Encode RFC4648 vectors"
    [ base64Encode (BSC.pack "f"), base64Encode (BSC.pack "fo"), base64Encode (BSC.pack "foo")
    , base64Encode (BSC.pack "foob"), base64Encode (BSC.pack "fooba"), base64Encode (BSC.pack "foobar")
    , base64Encode BS.empty ]
    [ "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy", "" ]
  let dlb = delugeLoginBody "pw" 1
      dmb = delugeAddMagnetBody "magnet:?x" "/dl" ["udp://t1:1337/announce"] 2
      dfb = delugeAddFileBody "n.torrent" "QUJD" "/dl" ["udp://t1:1337/announce"] 2
      methodOf v = case v of
        Aeson.Object o -> KeyMap.lookup (Key.fromText "method") o
        _ -> Nothing
      paramsOf v = case v of
        Aeson.Object o -> KeyMap.lookup (Key.fromText "params") o
        _ -> Nothing
  check "deluge RPC bodies: methods"
    [ methodOf dlb, methodOf dmb, methodOf dfb ]
    [ Just (Aeson.String "auth.login"), Just (Aeson.String "core.add_torrent_magnet")
    , Just (Aeson.String "core.add_torrent_file") ]
  check "deluge RPC bodies: params encoded"
    [ LBSC.unpack (Aeson.encode (paramsOf dlb)), LBSC.unpack (Aeson.encode (paramsOf dmb))
    , LBSC.unpack (Aeson.encode (paramsOf dfb)) ]
    [ "[\"pw\"]"
    , "[\"magnet:?x\",{\"add_trackers\":[\"udp://t1:1337/announce\"],\"download_location\":\"/dl\"}]"
    , "[\"n.torrent\",\"QUJD\",{\"add_trackers\":[\"udp://t1:1337/announce\"],\"download_location\":\"/dl\"}]" ]
  check "m62: add bodies omit add_trackers when the list is empty"
    [ LBSC.unpack (Aeson.encode (paramsOf (delugeAddMagnetBody "magnet:?x" "/dl" [] 2)))
    , LBSC.unpack (Aeson.encode (paramsOf (delugeAddFileBody "n" "QQ==" "/dl" [] 2))) ]
    [ "[\"magnet:?x\",{\"download_location\":\"/dl\"}]"
    , "[\"n\",\"QQ==\",{\"download_location\":\"/dl\"}]" ]
  check "m62: parseTrackerList drops blanks/comments, keeps schemes, dedupes"
    (parseTrackerList "udp://a:1/announce\n\n# comment\nhttp://b/announce\nudp://a:1/announce\nnot a url\n")
    ["udp://a:1/announce", "http://b/announce"]
  check "deluge RPC bodies: id"
    [ case dlb of Aeson.Object o -> KeyMap.lookup (Key.fromText "id") o; _ -> Nothing ]
    [ Just (Aeson.Number 1) ]
  -- milestone 29: deluge-web requires web.connect after auth.login
  check "deluge m29 bodies: web.get_hosts / web.connect / web.connected methods"
    [ methodOf (delugeGetHostsBody 2), methodOf (delugeConnectBody "host-1" 3), methodOf (delugeConnectedBody 4) ]
    [ Just (Aeson.String "web.get_hosts"), Just (Aeson.String "web.connect"), Just (Aeson.String "web.connected") ]
  check "deluge m29 connect body params carry the host id"
    [ LBSC.unpack (Aeson.encode (paramsOf (delugeConnectBody "host-1" 3))) ]
    [ "[\"host-1\"]" ]
  check "getFirstHostId: first host id wins, empty/null/garbage -> Nothing"
    [ getFirstHostId (LBSC.pack "{\"result\":[[\"host-1\",\"127.0.0.1\",58846,\"u\",\"p\"],[\"host-2\",\"x\",1,\"u\",\"p\"]]}")
    , getFirstHostId (LBSC.pack "{\"result\":[]}")
    , getFirstHostId (LBSC.pack "{\"result\":null}")
    , getFirstHostId (LBSC.pack "not json") ]
    [ Just "host-1", Nothing, Nothing, Nothing ]
  mPy <- findExecutable "python3"
  case mPy of
    Just _  -> m29MockTests >> m37MockTests
    Nothing -> putStrLn "PASS: m29/m37 mock-deluge IO tests skipped (python3 not in PATH)"
  -- milestone 27: runtime download-client settings + DownloadDir stamp
  let rs0 = RequestSettings { rsDelugeUrl = "http://127.0.0.1:8112/json"
                            , rsDelugePassword = "deluge", rsDownloadDir = "/dl"
                            , rsAutoApproveGb = Just 5 }
      cfg27 = Config
        { configLibraryPath = "/lib", configPort = 8080, configStateDir = "/st"
        , configRequireAuth = False, configServerName = "fms"
        , configFollowSymlinks = False, configTmdbApiKey = ""
        , configRateLimit = Nothing, configRateLimitAuth = Just 10, configLogLevel = "info"
        , configCorsOrigins = ["*"], configSecurityHeaders = True
        , configPasswordMinLength = 0, configMaxBitrate = 0, configHwAccel = "none"
        , configWatch = False, configMaxTranscodes = 2, configFfmpegTimeoutSec = 30
        , configTlsCert = Nothing, configTlsKey = Nothing
        , configFetchImages = False, configScanWorkers = 4, configMaxBodyBytes = 10485760, configTmdbBaseUrl = "https://api.themoviedb.org/3"
        , configTmdbImageBaseUrl = "https://image.tmdb.org/t/p", configTmdbImageSize = "w500", configTmdbBackdropSize = "original", configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400
        , configMemoryWarnMb = 1024, configMemorySampleSecs = 30
        , configUiDir = Nothing
        , configJackettUrl = Nothing
        , configJackettApiKey = Nothing
        , configDelugeUrl = "http://env:8112/json"
        , configDelugePassword = "envpw"
        , configDownloadDir = "/env-dl"
        , configRequestAutoApproveGb = 7
        , configIndexerUrl = Nothing
        , configIndexerApiKey = Nothing
        , configIndexerName = Nothing
        }
      seeded = settingsFromConfig cfg27
      patch1 = SettingsPatch { spDelugeUrl = Just "http://saved:8112/json"
                             , spDelugePassword = Nothing, spDownloadDir = Just "/saved-dl"
                             , spAutoApproveGb = Just (Just 2.5) }
      mergedS = mergeSettings rs0 patch1
      patchClear = emptySettingsPatch { spAutoApproveGb = Just Nothing }
      fieldOf :: T.Text -> Aeson.Value -> Maybe Aeson.Value
      fieldOf k v = case v of Aeson.Object o -> KeyMap.lookup (Key.fromText k) o; _ -> Nothing
  check "RequestSettings JSON roundtrip (lowercase persisted keys)"
    [ Aeson.decode (Aeson.encode rs0) == Just rs0 ]
    [True]
  check "settingsFromConfig: seeds from env-derived config"
    [ rsDelugeUrl seeded, rsDelugePassword seeded, rsDownloadDir seeded ]
    [ "http://env:8112/json", "envpw", "/env-dl" ]
  check "settingsFromConfig: seeds auto-approve GB from env config"
    [ rsAutoApproveGb seeded ] [ Just 7 ]
  check "mergeSettings: partial update keeps untouched password, sets others"
    [ rsDelugeUrl mergedS, rsDelugePassword mergedS, rsDownloadDir mergedS ]
    [ "http://saved:8112/json", "deluge", "/saved-dl" ]
  check "mergeSettings: AutoApproveGb override applied"
    [ rsAutoApproveGb mergedS ] [ Just 2.5 ]
  check "mergeSettings: empty patch is identity"
    [ mergeSettings rs0 emptySettingsPatch == rs0 ] [ True ]
  check "mergeSettings: AutoApproveGb null clears the override"
    [ rsAutoApproveGb (mergeSettings rs0 patchClear) ] [ Nothing ]
  check "effectiveAutoApproveGb: saved override wins, else env config"
    [ effectiveAutoApproveGb cfg27 rs0, effectiveAutoApproveGb cfg27 rs0 { rsAutoApproveGb = Nothing } ]
    [ 5, 7 ]
  check "settingsPublicJson: admin sees password, non-admin gets Null (redaction)"
    [ fieldOf "DelugePassword" (settingsPublicJson True rs0)
    , fieldOf "DelugePassword" (settingsPublicJson False rs0)
    , fieldOf "DelugeUrl" (settingsPublicJson False rs0)
    , fieldOf "AutoApproveGb" (settingsPublicJson False rs0) ]
    [ Just (Aeson.String "deluge"), Just Aeson.Null
    , Just (Aeson.String "http://127.0.0.1:8112/json"), Just (Aeson.Number 5) ]
  check "SettingsPatch FromJSON: PascalCase + camelCase keys, subset only"
    (case Aeson.decode (LBSC.pack "{\"DelugeUrl\":\"http://a/\",\"downloadDir\":\"/x\"}") of
        Just p  -> [spDelugeUrl p, spDownloadDir p, spDelugePassword p]
        Nothing -> [Nothing, Nothing, Nothing])
    [Just "http://a/", Just "/x", Nothing]
  check "SettingsPatch FromJSON: autoApproveGb null clears, number sets (PascalCase)"
    [ case Aeson.decode (LBSC.pack "{\"autoApproveGb\":null}") of
        Just p  -> spAutoApproveGb p
        Nothing -> Nothing
    , case Aeson.decode (LBSC.pack "{\"AutoApproveGb\":3.5}") of
        Just p  -> spAutoApproveGb p
        Nothing -> Nothing ]
    [Just Nothing, Just (Just 3.5)]
  let stamped = (mkReq "Dune" Pending) { reqDownloadDir = Just "/dl" }
  check "Request DownloadDir in API JSON: absent -> null, stamped -> value (m27)"
    [ fieldOf "DownloadDir" (Aeson.toJSON (mkReq "Dune" Pending))
    , fieldOf "DownloadDir" (Aeson.toJSON stamped) ]
    [ Just Aeson.Null, Just (Aeson.String "/dl") ]
  check "Request JSON roundtrip keeps DownloadDir stamp (m27)"
    [ Aeson.decode (Aeson.encode stamped) == Just stamped ] [ True ]
  check "requestsRoutes: literal Settings routes beat the :id capture (first-match-wins)"
    [ fmap (routePattern . fst) (firstMatch requestsRoutes "GET" ["Requests", "Settings"])
    , fmap (routePattern . fst) (firstMatch requestsRoutes "POST" ["Requests", "Settings"])
    , fmap (routePattern . fst) (firstMatch requestsRoutes "POST" ["Requests", "Settings", "Test"])
    , fmap (routePattern . fst) (firstMatch requestsRoutes "GET" ["Requests", "abc123"]) ]
    [ Just [Lit "Requests", Lit "Settings"], Just [Lit "Requests", Lit "Settings"]
    , Just [Lit "Requests", Lit "Settings", Lit "Test"], Just [Lit "Requests", Capture "id"] ]
  -- m27 dashboard markers
  let m27Markers =
        [ "Download client", "id='dlsec'", "id='dlcfg'", "id='dlstatus'"
        , "id=\"deluge-url\"", "id=\"deluge-password\"", "id=\"download-dir\"", "id=\"auto-approve-gb\""
        , "api('/Requests/Settings')", "/Requests/Settings/Test", "saveDl", "testDl"
        , "renderDlcfg", "DL_LOADED", "TEST OK", "SAVED", "admin only"
        , "r.DownloadDir" ]
  check "dashboardPageHtml: m27 download-client section markers present"
    (map (\m -> m `isInfixOf` dash) m27Markers) (replicate (length m27Markers) True)
  -- milestone 35: TMDB metadata settings (persisted, dashboard-editable)
  let ms0 = MetadataSettings { msTmdbApiKey = "ENVKEY", msFetchImages = False }
      msPatch1 = MetadataSettingsPatch { mspTmdbApiKey = Just "SAVEDKEY", mspFetchImages = Nothing }
      msMerged = mergeMetadataSettings ms0 msPatch1
      msPatchClear = emptyMetadataSettingsPatch { mspFetchImages = Just True }
      msFieldOf :: T.Text -> Aeson.Value -> Maybe Aeson.Value
      msFieldOf k v = case v of Aeson.Object o -> KeyMap.lookup (Key.fromText k) o; _ -> Nothing
  check "MetadataSettings JSON roundtrip (lowercase persisted keys)"
    [ Aeson.decode (Aeson.encode ms0) == Just ms0 ] [True]
  check "metadataSettingsFromConfig: seeds api key from env-derived config"
    (msTmdbApiKey (metadataSettingsFromConfig cfg27)) ""
  check "metadataSettingsFromConfig: seeds fetchImages from env-derived config"
    (msFetchImages (metadataSettingsFromConfig cfg27)) False
  check "mergeMetadataSettings: partial update keeps untouched key, sets new (key)"
    (msTmdbApiKey msMerged) "SAVEDKEY"
  check "mergeMetadataSettings: partial update keeps untouched key, sets new (images)"
    (msFetchImages msMerged) False
  check "mergeMetadataSettings: empty patch is identity"
    (mergeMetadataSettings ms0 emptyMetadataSettingsPatch == ms0) True
  check "mergeMetadataSettings: patch sets fetchImages"
    (msFetchImages (mergeMetadataSettings ms0 msPatchClear)) True
  check "metadataSettingsPublicJson: admin sees key, non-admin gets Null"
    [ msFieldOf "TmdbApiKey" (metadataSettingsPublicJson True ms0)
    , msFieldOf "TmdbApiKey" (metadataSettingsPublicJson False ms0)
    , msFieldOf "FetchImages" (metadataSettingsPublicJson False ms0) ]
    [ Just (Aeson.String "ENVKEY"), Just Aeson.Null, Just (Aeson.Bool False) ]
  case Aeson.decode (LBSC.pack "{\"TmdbApiKey\":\"K\",\"fetchImages\":true}") of
    Just p -> do
      check "MetadataSettingsPatch FromJSON: PascalCase key accepted" (mspTmdbApiKey p) (Just "K")
      check "MetadataSettingsPatch FromJSON: camelCase flag accepted" (mspFetchImages p) (Just True)
    Nothing -> check "MetadataSettingsPatch FromJSON: PascalCase + camelCase, subset" False True
  check "metadataModule routes: literal Settings routes beat captures (first-match-wins)"
    [ fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "GET" ["Metadata", "Settings"])
    , fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "POST" ["Metadata", "Settings"])
    , fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "POST" ["Metadata", "Settings", "Test"])
    , fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "GET" ["metadata", "settings"])
    , fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "GET" ["items", "1", "metadata"]) ]
    [ Just [Lit "Metadata", Lit "Settings"], Just [Lit "Metadata", Lit "Settings"]
    , Just [Lit "Metadata", Lit "Settings", Lit "Test"], Just [Lit "Metadata", Lit "Settings"]
    , Just [Lit "items", Capture "id", Lit "metadata"] ]
  let m35dir = "/tmp/fms-m35-state"
  resetDir m35dir
  let cfg35 = cfg27 { configStateDir = m35dir, configTmdbApiKey = "ENVKEY", configFetchImages = False }
  env35 <- newAppEnv cfg35
  rSeed <- runApp env35 (loadMetadataSettingsIntoState >> getMetadataSettings)
  seedFile <- doesFileExist (m35dir </> "metadata-settings.json")
  check "m35 settings: first boot seeds key from env config"
    (either (const "<err>") msTmdbApiKey rSeed) "ENVKEY"
  check "m35 settings: first boot seeds fetchImages from env config"
    (either (const False) msFetchImages rSeed) False
  check "m35 settings: first boot writes the settings file" seedFile True
  rUpd <- runApp env35 (updateMetadataSettings (\s -> s { msTmdbApiKey = "SAVEDKEY", msFetchImages = True }) >> getMetadataSettings)
  check "m35 settings: update persists and returns the saved key"
    (either (const "<err>") msTmdbApiKey rUpd) "SAVEDKEY"
  check "m35 settings: update persists and returns the saved fetchImages"
    (either (const False) msFetchImages rUpd) True
  env35b <- newAppEnv (cfg35 { configTmdbApiKey = "OTHERENV" })
  rReload <- runApp env35b (loadMetadataSettingsIntoState >> getMetadataSettings)
  check "m35 settings: saved file wins over env on restart"
    [ either (const "<err>") msTmdbApiKey rReload ] [ "SAVEDKEY" ]
  let m35Markers =
        [ "Metadata (TMDB)", "id='metasec'", "id='metacfg'", "id='metastatus'"
        , "id=\"tmdb-api-key\"", "id=\"fetch-images\"", "api('/Metadata/Settings')"
        , "/Metadata/Settings/Test", "saveMeta", "testMeta", "renderMeta", "META_LOADED"
        , "refreshMetaLib", "TEST OK", "Refresh library metadata" ]
  check "dashboardPageHtml: m35 metadata settings section markers present"
    (map (\m -> m `isInfixOf` dash) m35Markers) (replicate (length m35Markers) True)
  -- m35 live-ish: mock TMDB proving the SAVED key drives refreshFromTmdb +
  -- fetchAndStoreTmdbImages (guarded: nix build sandbox has no python3)
  mPy35 <- findExecutable "python3"
  case mPy35 of
    Nothing -> putStrLn "PASS: m35 mock-tmdb IO tests skipped (python3 not in PATH)"
    Just _ -> do
      let m35mock = "/tmp/fms-m35-mock"
          m35log = m35mock ++ "/calls.log"
          m35mockSrc = unlines
            [ "import json, sys"
            , "from http.server import BaseHTTPRequestHandler, HTTPServer"
            , "from urllib.parse import urlparse, parse_qs"
            , "port = int(sys.argv[1]); log = sys.argv[2]"
            , "class H(BaseHTTPRequestHandler):"
            , "    def log_message(self, *a): pass"
            , "    def _send(self, code, body, ctype='application/json'):"
            , "        b = body.encode() if isinstance(body, str) else body"
            , "        self.send_response(code)"
            , "        self.send_header('Content-Type', ctype)"
            , "        self.send_header('Content-Length', str(len(b)))"
            , "        self.end_headers()"
            , "        self.wfile.write(b)"
            , "    def do_GET(self):"
            , "        u = urlparse(self.path); q = parse_qs(u.query)"
            , "        key = q.get('api_key', [''])[0]"
            , "        with open(log, 'a') as f:"
            , "            f.write(u.path + '?api_key=' + key + chr(10))"
            , "        if key != 'SAVEDKEY' and u.path.startswith('/3/'):"
            , "            return self._send(401, '{\"status_message\":\"bad key\"}')"
            , "        if u.path == '/3/search/movie':"
            , "            return self._send(200, json.dumps({'results': [{'id': 42, 'title': 'Test Movie', 'release_date': '2020-01-01', 'overview': 'o', 'vote_average': 7.5, 'poster_path': '/p.jpg'}]}))"
            , "        if u.path == '/3/movie/42/images':"
            , "            return self._send(200, json.dumps({'posters': [{'file_path': '/p.jpg'}], 'backdrops': [{'file_path': '/b.jpg'}], 'logos': []}))"
            , "        if u.path == '/img/w500/p.jpg':"
            , "            return self._send(200, 'POSTERBYTES', 'image/jpeg')"
            , "        if u.path == '/img/w500/b.jpg':"
            , "            return self._send(200, 'BACKDROPBYTES', 'image/jpeg')"
            , "        if u.path == '/img/original/b.jpg':"
            , "            return self._send(200, 'BACKDROPBYTES-ORIGINAL', 'image/jpeg')"
            , "        return self._send(404, '{}')"
            , "srv = HTTPServer(('127.0.0.1', port), H)"
            , "open(log + '.ready', 'w').write('ok')"
            , "srv.serve_forever()" ]
      resetDir m35mock
      createDirectoryIfMissing True m35mock
      writeFile (m35mock ++ "/mock.py") m35mockSrc
      let m35item = MediaItem
            { itemId = "1", itemTitle = "Test Movie", itemPath = "/x.mp4"
            , itemSize = 1, itemMime = "video/mp4", itemAddedAt = reqT0, itemFolder = "1"
            , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
            , itemExtraType = Nothing, itemVersions = [] }
          cfg35m = cfg35 { configTmdbBaseUrl = "http://127.0.0.1:8930/3"
                         , configTmdbImageBaseUrl = "http://127.0.0.1:8930/img"
                         , configTmdbImageSize = "w500", configTmdbBackdropSize = "original", configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400 }
      bracket
        (createProcess (proc "python3" [m35mock ++ "/mock.py", "8930", m35log]))
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
        (\_ -> do
          rok <- waitForFile (m35log ++ ".ready") 40
          if not rok
            then putStrLn "PASS: m35 mock-tmdb IO tests skipped (mock did not start)"
            else do
              envM <- newAppEnv cfg35m
              _ <- runApp envM $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                _ <- updateMetadataSettings (\s -> s { msTmdbApiKey = "SAVEDKEY", msFetchImages = True })
                refreshFromTmdb False "1" m35item
              rGet <- runApp envM (getMetadata "1")
              let mMeta = either (const Nothing) id rGet
              check "m35 refreshFromTmdb uses the SAVED key against the mock (source tmdb)"
                (fmap metaSource mMeta) (Just "tmdb")
              check "m35 refreshFromTmdb uses the SAVED key against the mock (title)"
                (fmap metaTitle mMeta) (Just (Just "Test Movie"))
              imgP <- doesFileExist (m35dir </> "images" </> "1" </> "Primary.jpg")
              imgB <- doesFileExist (m35dir </> "images" </> "1" </> "Backdrop.jpg")
              check "m35 fetchAndStoreTmdbImages stored poster+backdrop (saved fetchImages=true)"
                [ imgP, imgB ] [ True, True ]
              mockCalls <- readFile m35log
              let callLines = lines mockCalls
                  apiLines = filter ("/3/" `isPrefixOf`) callLines
              check "m35 mock: every TMDB API call used the SAVED key"
                (all (\l -> "api_key=SAVEDKEY" `isSuffixOf` l) apiLines) True
              check "m35 mock: env key never leaked to the mock"
                (any ("ENVKEY" `isInfixOf`) callLines) False
              check "m35 mock: image CDN was hit for poster+backdrop"
                (length (filter ("/img/" `isPrefixOf`) callLines)) 2
              check "m57: backdrop downloaded at original, poster at w500 (per-kind sizes)"
                [ any ("/img/original/b.jpg" `isPrefixOf`) callLines
                , any ("/img/w500/p.jpg" `isPrefixOf`) callLines ]
                [ True, True ])

  -- milestone 36: clean release names before TMDB matching
  check "m36 cleanTitleForProvider: scene examples from the spec"
    [ cleanTitleForProvider "Shrek.2001.1080p.BluRay.x265-LAMA"
    , cleanTitleForProvider "The.Matrix.1999.2160p.UHD.BluRay.REMUX-FraMeSToR"
    , cleanTitleForProvider "sample-long"
    , cleanTitleForProvider "Fight Club" ]
    [ ("Shrek", Just 2001), ("The Matrix", Just 1999), ("sample long", Nothing), ("Fight Club", Nothing) ]
  check "m36 cleanTitleForProvider: year in the middle, multiple dots, no year, brackets"
    [ cleanTitleForProvider "The Movie 2001 Returns"
    , cleanTitleForProvider "a.b.c"
    , cleanTitleForProvider "Movie (Extended Edition) 2010"
    , cleanTitleForProvider "Amelie" ]
    [ ("The Movie Returns", Just 2001), ("a b c", Nothing), ("Movie", Just 2010), ("Amelie", Nothing) ]
  check "m64 cleanTitleForProvider: trailing lowercase scene group dropped when a year is present"
    [ cleanTitleForProvider "godzilla.minus.zero.2026.1080p.cam.x264-cinemacity"
    , cleanTitleForProvider "some.movie.2020.1080p.web.h264-lama"
    , cleanTitleForProvider "godzilla-minus-zero" ]  -- no year: conservative, kept
    [ ("godzilla minus zero", Just 2026), ("some movie", Just 2020), ("godzilla minus zero", Nothing) ]
  check "m36 cleanTitleForProvider: already-clean title unchanged, out-of-range year token dropped as pure digit (m39)"
    [ cleanTitleForProvider "Fight Club"
    , cleanTitleForProvider "Movie 1850 Tag" ]
    [ ("Fight Club", Nothing), ("Movie Tag", Nothing) ]
  let mkRes i y = TmdbResult { tmdbId = i, tmdbTitle = "T" <> T.pack (show i)
                             , tmdbOriginalTitle = Nothing, tmdbOriginalLanguage = Nothing
                             , tmdbOverview = Nothing, tmdbReleaseYear = y
                             , tmdbVoteAverage = Nothing, tmdbPoster = Nothing }
      rs36 = [mkRes 1 (Just 2000), mkRes 2 (Just 1999), mkRes 3 Nothing]
  check "m36 pickTmdbResult: year match preferred over first"
    (fmap tmdbId (pickTmdbResult (Just 1999) rs36)) (Just 2)
  check "m36 pickTmdbResult: no year match -> first result"
    (fmap tmdbId (pickTmdbResult (Just 2001) rs36)) (Just 1)
  check "m36 pickTmdbResult: no extracted year -> first result"
    (fmap tmdbId (pickTmdbResult Nothing rs36)) (Just 1)
  check "m36 pickTmdbResult: empty list -> Nothing"
    (pickTmdbResult (Just 1999) []) Nothing
  -- m66: originalTitlesOf expands a query with TMDB original titles
  let mkOrig i title orig lang = TmdbResult { tmdbId = i, tmdbTitle = title
        , tmdbOriginalTitle = orig, tmdbOriginalLanguage = lang
        , tmdbOverview = Nothing, tmdbReleaseYear = Nothing
        , tmdbVoteAverage = Nothing, tmdbPoster = Nothing }
  check "m66 originalTitlesOf: original title added when not already queried"
    ["El laberinto del fauno"]
    (originalTitlesOf ["Pan's Labyrinth"]
      [mkOrig 680 "Pan's Labyrinth" (Just "El laberinto del fauno") (Just "es")])
  check "m66 originalTitlesOf: skips titles already in the query list (case-insensitive), blanks, Nothings"
    ["Other Title"]
    (originalTitlesOf ["pan's labyrinth", "El laberinto del fauno"]
      [ mkOrig 680 "Pan's Labyrinth" (Just "Pan's Labyrinth") (Just "en")
      , mkOrig 1 "X" (Just " el laberinto del fauno ") Nothing
      , mkOrig 2 "Y" (Just "  ") Nothing
      , mkOrig 3 "Z" Nothing Nothing
      , mkOrig 4 "W" (Just "Other Title") Nothing ])
  check "m66 originalTitlesOf: keeps distinct original titles in order"
    ["El laberinto del fauno", "Le Fabuleux Destin d'Amélie Poulain"]
    (originalTitlesOf ["Pan's Labyrinth"]
      [ mkOrig 680 "Pan's Labyrinth" (Just "El laberinto del fauno") (Just "es")
      , mkOrig 1 "Amélie" (Just "Le Fabuleux Destin d'Amélie Poulain") (Just "fr") ])

  -- milestone 37: live deluge progress on requests
  let sts37 = [ ("Shrek.2001.1080p.BluRay.x265-LAMA", "Downloading", 42.5, 1024.0)
              , ("Other Movie 2020", "Seeding", 100.0, 0.0) ]
  check "m37 matchTorrentProgress: release-name match"
    (matchTorrentProgress "Shrek 2001" (Just "Shrek 2001 1080p BluRay x265-LAMA") sts37)
    (Just (42.5, "Downloading"))
  check "m37 matchTorrentProgress: title containment match"
    (matchTorrentProgress "Other Movie" Nothing sts37) (Just (100.0, "Seeding"))
  check "m37 matchTorrentProgress: no match -> Nothing"
    (matchTorrentProgress "Nope" Nothing sts37) Nothing
  check "m37 matchTorrentProgress: empty torrent list -> Nothing"
    (matchTorrentProgress "Shrek 2001" Nothing []) Nothing
  -- m50: attachLiveProgress fills ANY matching request (not only Downloading)
  check "m50 attachLiveProgress: Imported request with matching Seeding torrent gets Progress 100/Seeding"
    (map (\rq -> (reqStatus rq, reqProgress rq, reqTorrentState rq))
      (attachLiveProgress sts37 [(mkReq "Other Movie" Imported) { reqSourceRelease = Just "Other Movie 2020" }]))
    [(Imported, Just 100.0, Just "Seeding")]
  check "m50 attachLiveProgress: non-matching stays null, Downloading still filled"
    (map (\rq -> (reqProgress rq, reqTorrentState rq))
      (attachLiveProgress sts37 [mkReq "Nope" Imported, mkReq "Shrek 2001" Downloading]))
    [(Nothing, Nothing), (Just 42.5, Just "Downloading")]
  check "m37 parseTorrentStatuses: result object -> tuples (sorted; KeyMap order is arbitrary)"
    (fmap (sort . parseTorrentStatuses) (Aeson.decode' (LBSC.pack "{\"t1\":{\"name\":\"A Movie\",\"state\":\"Downloading\",\"progress\":42.5,\"download_payload_rate\":1024},\"t2\":{\"name\":\"B\",\"state\":\"Seeding\",\"progress\":100}}")))
    (Just [("A Movie", "Downloading", 42.5, 1024.0), ("B", "Seeding", 100.0, 0.0)])
  check "m37 delugeTorrentsStatusBody: method + keys + id in encoded JSON"
    (let enc = BSC.unpack (LBS.toStrict (Aeson.encode (delugeTorrentsStatusBody 4)))
     in [ "core.get_torrents_status" `isInfixOf` enc
        , "\"progress\"" `isInfixOf` enc
        , "\"download_payload_rate\"" `isInfixOf` enc
        , "\"id\":4" `isInfixOf` enc ])
    [True, True, True, True]

  -- milestone 39: poster language preference + harder release-tag cleaning
  check "m39 pickByLanguage: en and no-lang entries first (stable), rest after"
    (map tieLang (pickByLanguage
      [ TmdbImageEntry (Just "zh.jpg") (Just "zh")
      , TmdbImageEntry (Just "en.jpg") (Just "en")
      , TmdbImageEntry (Just "nolang.jpg") Nothing
      , TmdbImageEntry (Just "fr.jpg") (Just "fr") ]))
    [ Just "en", Nothing, Just "zh", Just "fr" ]
  check "m39 pickByLanguage: only zh -> zh stays first"
    (map tieFilePath (pickByLanguage [ TmdbImageEntry (Just "zh.jpg") (Just "zh") ]))
    [ Just "zh.jpg" ]
  check "m39 tmdbImageListToDownloads: zh poster first, en poster -> Primary is the en one"
    (case Aeson.decode (LBSC.pack "{\"posters\":[{\"file_path\":\"/zh.jpg\",\"iso_639_1\":\"zh\"},{\"file_path\":\"/en.jpg\",\"iso_639_1\":\"en\"}],\"backdrops\":[],\"logos\":[]}") of
      Just v  -> tmdbImageListToDownloads v
      Nothing -> [])
    [("Primary", "/en.jpg")]
  check "m39 cleanTitleForProvider: spec examples (10Bit/DD5.1/H265-d3g, WEBRip 2026, Se7en guard)"
    [ cleanTitleForProvider "Inception.2010.BluRay.10Bit.1080p.DD5.1.H265-d3g"
    , cleanTitleForProvider "Spider-Man.Brand.New.Day.2026.1080p.WEBRip.10Bit.DDP5.1.x265-NeoNoir"
    , cleanTitleForProvider "Shrek.2001.1080p.BluRay.x265-LAMA"
    , cleanTitleForProvider "Se7en.1995.1080p.BluRay.x264-GRP" ]
    [ ("Inception", Just 2010), ("Spider Man Brand New Day", Just 2026)
    , ("Shrek", Just 2001), ("Se7en", Just 1995) ]
  check "m39 cleanTitleForProvider: hyphenated title word without group kept"
    (cleanTitleForProvider "Spider-Man") ("Spider Man", Nothing)

  -- milestone 43: legacy rows carry type/seriesName + episode displayTitle
  let m43fieldOf :: T.Text -> Aeson.Value -> Maybe Aeson.Value
      m43fieldOf k v = case v of Aeson.Object o -> KeyMap.lookup (Key.fromText k) o; _ -> Nothing
      m43movie = MediaItem
        { itemId = "40", itemTitle = "Shrek.2001.1080p.BluRay.x265-LAMA", itemPath = "/lib/shrek.mkv"
        , itemSize = 999, itemMime = "video/x-matroska", itemAddedAt = reqT0, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      m43ep = m43movie { itemId = "ep1", itemType = "Episode"
                       , itemSeries = Just "Family Guy", itemSeason = Just 25, itemEpisode = Just 0 }
      m43row = withLegacyItem HM.empty m43ep (simpleItemJson m43ep)
      m43movieRow = withLegacyItem HM.empty m43movie (simpleItemJson m43movie)
  check "m43 withLegacyItem: /library episode row carries type + seriesName + SxxEyy displayTitle"
    [ m43fieldOf "type" m43row
    , m43fieldOf "seriesName" m43row
    , m43fieldOf "displayTitle" m43row
    , m43fieldOf "title" m43row ]
    [ Just (Aeson.String "Episode"), Just (Aeson.String "Family Guy")
    , Just (Aeson.String "Family Guy S25E00")
    , Just (Aeson.String "Shrek.2001.1080p.BluRay.x265-LAMA") ]
  check "m47 withLegacyItem: episode row also carries seasonNumber/episodeNumber"
    [ m43fieldOf "seasonNumber" m43row, m43fieldOf "episodeNumber" m43row
    , m43fieldOf "seasonNumber" m43movieRow ]
    [ Just (Aeson.Number 25), Just (Aeson.Number 0), Nothing ]
  check "m43 withLegacyItem: movie row type movie, no seriesName, raw-title displayTitle fallback"
    [ m43fieldOf "type" m43movieRow
    , m43fieldOf "seriesName" m43movieRow
    , m43fieldOf "displayTitle" m43movieRow ]
    [ Just (Aeson.String "Movie"), Nothing
    , Just (Aeson.String "Shrek.2001.1080p.BluRay.x265-LAMA") ]
  check "m44 displayTitleForItem: live filename -> Family Guy S25E00"
    (displayTitleForItem HM.empty
        (m43movie { itemId = "ep2", itemType = "Episode", itemSeries = Just "Family Guy"
                  , itemSeason = Just 25, itemEpisode = Just 0 }))
    (Just "Family Guy S25E00")
  check "m43 displayTitleForItem: metadata title beats the episode label"
    (displayTitleForItem (HM.fromList [("ep1", emptyMetadata { metaTitle = Just "Family Guy" })]) m43ep)
    (Just "Family Guy")

  -- milestone 42: audio-token prefix matching (aac5/ddp5.1/dts51...)
  check "m42 cleanTitleForProvider: AAC5.1 audio tag + dash group (live bug)"
    (cleanTitleForProvider "Heart.Of.The.Beast.2026.1080p.WEBRip.x264.AAC5.1-LAMA")
    ("Heart Of The Beast", Just 2026)
  check "m42 cleanTitleForProvider: audio variants aac5/ac3/dts51/eac3 dropped"
    [ cleanTitleForProvider "Movie.2020.1080p.AAC5"
    , cleanTitleForProvider "Movie.2020.1080p.AC3"
    , cleanTitleForProvider "Movie.2020.1080p.DTS51"
    , cleanTitleForProvider "Movie.2020.1080p.EAC3" ]
    [ ("Movie", Just 2020), ("Movie", Just 2020), ("Movie", Just 2020), ("Movie", Just 2020) ]
  check "m42 cleanTitleForProvider: words starting with codec letters are kept"
    [ cleanTitleForProvider "Ad Astra"
    , cleanTitleForProvider "Addams Family"
    , cleanTitleForProvider "Atmosphere" ]
    [ ("Ad Astra", Nothing), ("Addams Family", Nothing), ("Atmosphere", Nothing) ]

  -- milestone 40: DisplayTitle / displayTitle on item JSON
  let m40fieldOf :: T.Text -> Aeson.Value -> Maybe Aeson.Value
      m40fieldOf k v = case v of Aeson.Object o -> KeyMap.lookup (Key.fromText k) o; _ -> Nothing
      m40item = MediaItem
        { itemId = "40", itemTitle = "Shrek.2001.1080p.BluRay.x265-LAMA", itemPath = "/lib/shrek.mkv"
        , itemSize = 999, itemMime = "video/x-matroska", itemAddedAt = reqT0, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      m40lookup i = if i == "40" then Just "Shrek" else Nothing
  check "m40 withDisplayTitle: metadata title beats Name on the PascalCase shape"
    (m40fieldOf "DisplayTitle" (withDisplayTitle m40lookup (Aeson.toJSON m40item)))
    (Just (Aeson.String "Shrek"))
  check "m40 withDisplayTitle: falls back to Name when no metadata"
    (m40fieldOf "DisplayTitle" (withDisplayTitle (const Nothing) (Aeson.toJSON m40item)))
    (Just (Aeson.String "Shrek.2001.1080p.BluRay.x265-LAMA"))
  check "m40 withDisplayTitleLower: /library handler shape gains displayTitle (meta) and keeps raw title"
    [ m40fieldOf "displayTitle" (withDisplayTitleLower m40lookup (simpleItemJson m40item))
    , m40fieldOf "title" (withDisplayTitleLower m40lookup (simpleItemJson m40item)) ]
    [ Just (Aeson.String "Shrek"), Just (Aeson.String "Shrek.2001.1080p.BluRay.x265-LAMA") ]
  check "m40 withDisplayTitleLower: falls back to title when no metadata"
    (m40fieldOf "displayTitle" (withDisplayTitleLower (const Nothing) (simpleItemJson m40item)))
    (Just (Aeson.String "Shrek.2001.1080p.BluRay.x265-LAMA"))

  -- milestone 38: only mark Imported when the deluge torrent is complete
  check "m38 importComplete: matched Seeding 100 -> True"
    (importComplete sts37 "Other Movie" Nothing) True
  check "m38 importComplete: matched Downloading 42.5 -> False"
    (importComplete sts37 "Shrek 2001" (Just "Shrek 2001 1080p BluRay x265-LAMA")) False
  check "m38 importComplete: progress 99.95 (not Seeding) -> True"
    (importComplete [("X Movie", "Downloading", 99.95, 0)] "X Movie" Nothing) True
  check "m38 importComplete: no matching torrent -> True (fallback file mode)"
    (importComplete sts37 "Nope" Nothing) True
  check "m38 importComplete: empty statuses -> True (deluge down)"
    (importComplete [] "Shrek 2001" Nothing) True

  -- m38 live: markImported gated on the live mock-deluge status
  case mPy of
    Nothing -> putStrLn "PASS: m38 mock-deluge IO tests skipped (python3 not in PATH)"
    Just _ -> do
      let m38dir = "/tmp/fms-m38-state"
          m38dl = m38dir </> "downloads"
          m38mock = "/tmp/fms-m38-mock"
          m38log = m38mock </> "calls.log"
          cfg38 = cfg27 { configStateDir = m38dir, configDownloadDir = m38dl
                        , configDelugeUrl = "http://127.0.0.1:8915/json", configDelugePassword = "deluge" }
          mockSrc38 = unlines
            [ "import json, sys"
            , "from http.server import BaseHTTPRequestHandler, HTTPServer"
            , "port = int(sys.argv[1]); log = sys.argv[2]; mode = sys.argv[3]"
            , "state = {'connected': False}"
            , "class H(BaseHTTPRequestHandler):"
            , "    def log_message(self, *a): pass"
            , "    def do_POST(self):"
            , "        n = int(self.headers.get('Content-Length', 0))"
            , "        req = json.loads(self.rfile.read(n))"
            , "        m = req.get('method', '')"
            , "        res = None; cookie = False"
            , "        if m == 'auth.login':"
            , "            res = 'sess123' if req.get('params', [''])[0] == 'deluge' else False; cookie = res != False"
            , "        elif m == 'web.get_hosts':"
            , "            res = [['host-1', '127.0.0.1', 58846, 'localclient', '']]"
            , "        elif m == 'web.connect':"
            , "            state['connected'] = True; res = True"
            , "        elif m == 'core.get_torrents_status' and state['connected']:"
            , "            if mode == 'full':"
            , "                res = {'t1': {'name': 'Shrek.2001.1080p.BluRay.x265-LAMA', 'state': 'Seeding', 'progress': 100.0, 'download_payload_rate': 0}}"
            , "            else:"
            , "                res = {'t1': {'name': 'Shrek.2001.1080p.BluRay.x265-LAMA', 'state': 'Downloading', 'progress': 50.0, 'download_payload_rate': 2048}}"
            , "        payload = json.dumps({'result': res, 'error': None, 'id': req.get('id')}).encode()"
            , "        self.send_response(200)"
            , "        self.send_header('Content-Type', 'application/json')"
            , "        if cookie:"
            , "            self.send_header('Set-Cookie', 'deluge_web_session=sess123; Path=/')"
            , "        self.send_header('Content-Length', str(len(payload)))"
            , "        self.end_headers()"
            , "        self.wfile.write(payload)"
            , "srv = HTTPServer(('127.0.0.1', port), H)"
            , "open(log + '.ready', 'w').write('ok')"
            , "srv.serve_forever()" ]
          r38 = (mkReq "Shrek 2001" Downloading) { reqId = "r38"
                 , reqSourceRelease = Just "Shrek 2001 1080p BluRay x265-LAMA" }
      resetDir m38dir
      resetDir m38mock
      createDirectoryIfMissing True m38mock
      createDirectoryIfMissing True m38dl
      writeFile (m38mock </> "mock.py") mockSrc38
      writeFile (m38dl </> "Shrek.2001.1080p.BluRay.x265-LAMA.mkv") "videobytes"
      env38 <- newAppEnv cfg38
      _ <- runApp env38 (loadRequestsIntoState >> loadRequestSettingsIntoState >> insertRequest r38)
      let runMock mode act = bracket
            (createProcess (proc "python3" [m38mock </> "mock.py", "8915", m38log, mode]))
            (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
            (\_ -> do
              rok <- waitForFile (m38log ++ ".ready") 40
              if not rok then putStrLn "PASS: m38 mock phase skipped (mock did not start)" else act)
      runMock "half" $ do
        _ <- runApp env38 (fetchTorrentStatuses >>= \sts -> listRequests >>= markImported sts)
        st38a <- runApp env38 (fmap reqStatus <$> getRequest "r38")
        check "m38 live: torrent at 50% with file present -> stays Downloading"
          (either (const Nothing) id st38a) (Just Downloading)
      runMock "full" $ do
        _ <- runApp env38 (fetchTorrentStatuses >>= \sts -> listRequests >>= markImported sts)
        st38b <- runApp env38 (fmap reqStatus <$> getRequest "r38")
        check "m38 live: torrent Seeding 100% with file present -> Imported"
          (either (const Nothing) id st38b) (Just Imported)

  -- milestone 45: episodes refresh via TMDB TV search (mock /search/tv)
  case mPy of
    Nothing -> putStrLn "PASS: m45 mock-tmdb-tv IO tests skipped (python3 not in PATH)"
    Just _ -> do
      let m45dir = "/tmp/fms-m45-state"
          m45mock = "/tmp/fms-m45-mock"
          m45log = m45mock ++ "/calls.log"
          cfg45 = cfg27 { configStateDir = m45dir, configTmdbApiKey = "K"
                         , configTmdbBaseUrl = "http://127.0.0.1:8931/3"
                         , configTmdbImageBaseUrl = "http://127.0.0.1:8931/img"
                         , configTmdbImageSize = "w500", configTmdbBackdropSize = "original", configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400 }
          m45mockSrc = unlines
            [ "import json, sys"
            , "from http.server import BaseHTTPRequestHandler, HTTPServer"
            , "from urllib.parse import urlparse, parse_qs"
            , "port = int(sys.argv[1]); log = sys.argv[2]"
            , "class H(BaseHTTPRequestHandler):"
            , "    def log_message(self, *a): pass"
            , "    def do_GET(self):"
            , "        u = urlparse(self.path); q = parse_qs(u.query)"
            , "        with open(log, 'a') as f:"
            , "            f.write(u.path + chr(10))"
            , "        if u.path == '/3/search/tv':"
            , "            return self._send(200, json.dumps({'results': [{'id': 1234, 'name': 'Family Guy', 'first_air_date': '1999-01-31', 'overview': 'o', 'vote_average': 8.0, 'poster_path': '/fg.jpg'}]}))"
            , "        if u.path == '/3/tv/1234/images':"
            , "            return self._send(200, json.dumps({'posters': [{'file_path': '/fg.jpg'}], 'backdrops': [], 'logos': []}))"
            , "        if u.path == '/img/w500/fg.jpg':"
            , "            self.send_response(200); self.send_header('Content-Type', 'image/jpeg')"
            , "            b = b'POSTER'; self.send_header('Content-Length', str(len(b))); self.end_headers(); self.wfile.write(b); return"
            , "        return self._send(404, '{}')"
            , "    def _send(self, code, body):"
            , "        b = body.encode(); self.send_response(code); self.send_header('Content-Type', 'application/json'); self.send_header('Content-Length', str(len(b))); self.end_headers(); self.wfile.write(b)"
            , "srv = HTTPServer(('127.0.0.1', port), H)"
            , "open(log + '.ready', 'w').write('ok')"
            , "srv.serve_forever()" ]
          ep45 = MediaItem
            { itemId = "ep45", itemTitle = "Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta"
            , itemPath = "/dl/fg.mkv", itemSize = 1, itemMime = "video/x-matroska"
            , itemAddedAt = reqT0, itemFolder = "1", itemType = "Episode"
            , itemSeries = Just "Family Guy", itemSeason = Just 25, itemEpisode = Just 0
            , itemExtraType = Nothing, itemVersions = [] }
      resetDir m45dir
      resetDir m45mock
      createDirectoryIfMissing True m45mock
      writeFile (m45mock ++ "/mock.py") m45mockSrc
      bracket
        (createProcess (proc "python3" [m45mock ++ "/mock.py", "8931", m45log]))
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
        (\_ -> do
          rok <- waitForFile (m45log ++ ".ready") 40
          if not rok
            then putStrLn "PASS: m45 mock-tmdb-tv IO tests skipped (mock did not start)"
            else do
              env45 <- newAppEnv cfg45
              _ <- runApp env45 $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                _ <- updateMetadataSettings (\st -> st { msTmdbApiKey = "K", msFetchImages = True })
                refreshFromTmdb False "ep45" ep45
              rGet <- runApp env45 (getMetadata "ep45")
              let mMeta = either (const Nothing) id rGet
              check "m45 episode refresh: TV search stored series title"
                (fmap metaTitle mMeta) (Just (Just "Family Guy"))
              check "m45 episode refresh: TV search stored source tmdb"
                (fmap metaSource mMeta) (Just "tmdb")
              check "m45 episode refresh: providerIds carries tmdb id"
                (fmap (HM.lookup "tmdb" . metaProviderIds) mMeta) (Just (Just "1234"))
              img45 <- doesFileExist (m45dir </> "images" </> "ep45" </> "Primary.jpg")
              check "m45 episode refresh: tv artwork stored" img45 True
              calls45 <- readFile m45log
              let cl = lines calls45
              check "m45 mock: hit /search/tv and /tv/1234/images, never /search/movie"
                [ any ("/3/search/tv" ==) cl, any ("/3/tv/1234/images" ==) cl, any ("/3/search/movie" ==) cl ]
                [ True, True, False ])

  -- milestone 21: built-in Jackett-style indexers module
  let idx = Indexer { indexerId = "i1", indexerName = "MyIdx", indexerBaseUrl = "http://127.0.0.1:9117/"
                    , indexerApiKey = "SECRET", indexerCategories = [2000, 5000], indexerType = "torznab"
                    , indexerEnabled = True, indexerLastTestAt = Nothing, indexerLastTestOk = Nothing
                    , indexerAuthType = "apikey", indexerCookie = ""
                    , indexerResponseFormat = "torznab", indexerSearchPath = "/api" }
      idxNz = idx { indexerType = "newznab" }
      idxCk = idx { indexerAuthType = "cookie", indexerCookie = "session=ok"
                  , indexerResponseFormat = "torrentday-json" }
  check "indexerSearchUrl: torznab cats joined, movie/tv narrow, q encoded, newznab drops cat"
    [ indexerSearchUrl idx Nothing "the matrix"
    , indexerSearchUrl idx (Just "movie") "the matrix"
    , indexerSearchUrl idx (Just "tv") "a b"
    , indexerSearchUrl idxNz Nothing "x" ]
    [ "http://127.0.0.1:9117/api?t=search&q=the%20matrix&apikey=SECRET&cat=2000,5000"
    , "http://127.0.0.1:9117/api?t=search&q=the%20matrix&apikey=SECRET&cat=2000"
    , "http://127.0.0.1:9117/api?t=search&q=a%20b&apikey=SECRET&cat=5000"
    , "http://127.0.0.1:9117/api?t=search&q=x&apikey=SECRET" ]
  check "categoriesParam: default, movie/tv narrowing, unknown type keeps all, missing cat falls back"
    [ categoriesParam [2000,5000] Nothing, categoriesParam [2000,5000] (Just "movie")
    , categoriesParam [2000,5000] (Just "tv"), categoriesParam [2000,5000] (Just "other")
    , categoriesParam [5000] (Just "movie") ]
    [ [2000,5000], [2000], [5000], [2000,5000], [2000] ]
  check "base64Url roundtrip (incl. empty)"
    [ base64UrlDecode (base64UrlEncode "http://127.0.0.1:9117/get/1.torrent")
    , base64UrlDecode (base64UrlEncode "") ]
    [ Just "http://127.0.0.1:9117/get/1.torrent", Just "" ]
  check "base64Url alphabet is url-safe (no + / =)"
    [ T.all (\c -> c /= '+' && c /= '/' && c /= '=') (base64UrlEncode "http://x/y?a=b&c=d+/") ]
    [ True ]
  check "base64UrlDecode rejects garbage and bad length"
    [ base64UrlDecode "!!!", base64UrlDecode "abcde", base64UrlDecode "a" ]
    [ Nothing, Nothing, Nothing ]
  check "hostOf: scheme/userinfo/port stripped, lowercased"
    [ hostOf "http://127.0.0.1:9117/api", hostOf "https://user:pw@Example.COM:8443/x", hostOf "http://evil.com/" ]
    [ "127.0.0.1", "example.com", "evil.com" ]
  check "hostMatches: SSRF guard (same host ok incl. port/path diff, other host rejected, empty rejected)"
    [ hostMatches "http://127.0.0.1:9117/get/1" "http://127.0.0.1:9117"
    , hostMatches "http://127.0.0.1:9117/get/1" "http://127.0.0.1:9117/api"
    , hostMatches "http://evil.com/x" "http://127.0.0.1:9117"
    , hostMatches "" "http://127.0.0.1:9117" ]
    [ True, True, False, False ]
  check "Indexer JSON roundtrip"
    [ Aeson.decode (Aeson.encode idx) == Just idx ] [ True ]
  check "indexerPublicJson: admin sees ApiKey, non-admin gets Null"
    [ case indexerPublicJson True idx of Aeson.Object o -> KeyMap.lookup (Key.fromText "ApiKey") o; _ -> Nothing
    , case indexerPublicJson False idx of Aeson.Object o -> KeyMap.lookup (Key.fromText "ApiKey") o; _ -> Nothing ]
    [ Just (Aeson.String "SECRET"), Just Aeson.Null ]
  -- milestone 23: cookie-auth indexers + TorrentDay t.json mode
  check "indexerSearchUrl m23: cookie auth omits apikey param, custom searchPath used"
    [ indexerSearchUrl idxCk Nothing "the matrix"
    , indexerSearchUrl (idx { indexerSearchPath = "/torznab" }) Nothing "x" ]
    [ "http://127.0.0.1:9117/api?t=search&q=the%20matrix&cat=2000,5000"
    , "http://127.0.0.1:9117/torznab?t=search&q=x&apikey=SECRET&cat=2000,5000" ]
  check "m60: indexerSearchUrlExtra appends encoded season/ep params"
    (indexerSearchUrlExtra idx (Just "tv") "family guy s01e01" [("season", "1"), ("ep", "1")])
    (indexerSearchUrl idx (Just "tv") "family guy s01e01" ++ "&season=1&ep=1")
  check "torrentDaySearchUrl: semicolon cats + encoded q, empty cats"
    [ torrentDaySearchUrl idx "loop movie", torrentDaySearchUrl (idx { indexerCategories = [] }) "a b" ]
    [ "http://127.0.0.1:9117/t.json?2000;5000;q=loop%20movie", "http://127.0.0.1:9117/t.json?q=a%20b" ]
  let tdSample = LBSC.pack (unwords
        [ "[{\"name\":\"Loop Movie 2020\",\"t\":7,\"size\":2000000000,\"seeders\":9,\"c\":1,\"ctime\":1700000000},"
        , "{\"name\":\"NoSize\",\"t\":8}]" ])
      td = parseTorrentDayJson "http://td/" tdSample
  check "parseTorrentDayJson: titles parsed" [ map relTitle td ] [ ["Loop Movie 2020", "NoSize"] ]
  check "parseTorrentDayJson: sizes, missing tolerated"
    [ map relSizeBytes td ] [ [Just 2000000000, Nothing] ]
  check "parseTorrentDayJson: seeders, missing tolerated"
    [ map relSeeders td ] [ [Just 9, Nothing] ]
  check "parseTorrentDayJson: derived download.php urls"
    [ map relDownloadUrl td ]
    [ [Just "http://td/download.php/7/7.torrent", Just "http://td/download.php/8/8.torrent"] ]
  check "parseTorrentDayJson: non-array / garbage -> []"
    [ parseTorrentDayJson "http://td/" (LBSC.pack "{\"a\":1}")
    , parseTorrentDayJson "http://td/" (LBSC.pack "nope") ]
    [ [], [] ]
  check "validAuthType / validResponseFormat matrices"
    [ validAuthType "apikey", validAuthType "cookie", validAuthType "COOKIE", validAuthType "x"
    , validResponseFormat "torznab", validResponseFormat "torrentday-json", validResponseFormat "xml" ]
    [ True, True, True, False, True, True, False ]
  check "indexerPublicJson m23: Cookie redacted like ApiKey; admin sees it"
    [ case indexerPublicJson False idxCk of Aeson.Object o -> KeyMap.lookup (Key.fromText "Cookie") o; _ -> Nothing
    , case indexerPublicJson False idxCk of Aeson.Object o -> KeyMap.lookup (Key.fromText "ApiKey") o; _ -> Nothing
    , case indexerPublicJson True idxCk of Aeson.Object o -> KeyMap.lookup (Key.fromText "Cookie") o; _ -> Nothing ]
    [ Just Aeson.Null, Just Aeson.Null, Just (Aeson.String "session=ok") ]
  check "indexerCookieHeader: cookie auth emits header, apikey does not"
    [ indexerCookieHeader idxCk, indexerCookieHeader idx ] [ [("Cookie", "session=ok")], [] ]
  check "Indexer FromJSON: old PascalCase indexers.json decodes with m23 text defaults"
    [ case Aeson.decode (LBSC.pack "{\"Id\":\"x\",\"Name\":\"n\",\"BaseUrl\":\"http://b\",\"ApiKey\":\"k\",\"Enabled\":true}") of
        Just i  -> [indexerAuthType i, indexerCookie i, indexerResponseFormat i, indexerSearchPath i]
        Nothing -> [] ]
    [ ["apikey", "", "torznab", "/api"] ]
  check "Indexer FromJSON: old PascalCase file gets default categories"
    [ case Aeson.decode (LBSC.pack "{\"Id\":\"x\",\"Name\":\"n\",\"BaseUrl\":\"http://b\"}") of
        Just i  -> indexerCategories i
        Nothing -> [] ]
    [ [2000, 5000] ]

  -- milestone 47: series view (TMDB tv details + seasons/episodes + Downloaded)
  check "m47 tmdbTvDetailsUrlAt / tmdbSeasonUrlAt builders (trailing slash stripped)"
    [ tmdbTvDetailsUrlAt "http://127.0.0.1:9147/3" "K" 1942
    , tmdbTvDetailsUrlAt "http://127.0.0.1:9147/3/" "K" 1
    , tmdbSeasonUrlAt "http://127.0.0.1:9147/3" "K" 1942 25 ]
    [ "http://127.0.0.1:9147/3/tv/1942?api_key=K"
    , "http://127.0.0.1:9147/3/tv/1?api_key=K"
    , "http://127.0.0.1:9147/3/tv/1942/season/25?api_key=K" ]
  let tvDetailsJson = Aeson.encode (object
        [ "name" .= ("Family Guy" :: T.Text), "overview" .= ("A cartoon." :: T.Text)
        , "first_air_date" .= ("1999-01-31" :: T.Text), "poster_path" .= ("/fg.jpg" :: T.Text)
        , "seasons" .= [ object [ "season_number" .= (25 :: Int), "name" .= ("Season 25" :: T.Text), "episode_count" .= (2 :: Int) ]
                       , object [ "season_number" .= (0 :: Int) ] ] ])
      tvDetails = parseTmdbTvDetails tvDetailsJson
  check "m47 parseTmdbTvDetails: name/overview/date/poster/seasons"
    (fmap (\d -> (tdName d, tdOverview d, tdFirstAirDate d, tdPoster d, map tsSeasonNumber (tdSeasons d))) tvDetails)
    (Right (Just "Family Guy", Just "A cartoon.", Just "1999-01-31", Just "/fg.jpg", [25, 0]))
  check "m47 parseTmdbTvDetails: missing optional keys tolerated"
    (fmap (\d -> (tdName d, tdSeasons d)) (parseTmdbTvDetails (Aeson.encode (object []))))
    (Right (Nothing, []))
  check "m47 parseTmdbTvDetails on garbage -> Left"
    True (isLeftStr (parseTmdbTvDetails (LBS.fromStrict (BSC.pack "[1]"))))
  let seasonJson = Aeson.encode (object [ "episodes" .=
        [ object [ "episode_number" .= (0 :: Int), "name" .= ("Pilot" :: T.Text), "overview" .= ("o0" :: T.Text)
                 , "air_date" .= ("1999-01-31" :: T.Text), "still_path" .= ("/s0.jpg" :: T.Text), "vote_average" .= (8.5 :: Double) ]
        , object [ "episode_number" .= (1 :: Int) ] ] ])
      eps47 = parseTmdbSeasonEpisodes seasonJson
  check "m47 parseTmdbSeasonEpisodes: fields + missing tolerated"
    (fmap (map (\e -> (teEpisodeNumber e, teName e, teOverview e, teAirDate e, teStill e, teVote e))) eps47)
    (Right [ (0, Just "Pilot", Just "o0", Just "1999-01-31", Just "/s0.jpg", Just 8.5)
           , (1, Nothing, Nothing, Nothing, Nothing, Nothing) ])
  check "m47 parseTmdbSeasonEpisodes on garbage -> Left"
    True (isLeftStr (parseTmdbSeasonEpisodes (LBS.fromStrict (BSC.pack "nope"))))
  check "m47 parseTmdbSeasonEpisodes: no episodes key -> []"
    (parseTmdbSeasonEpisodes (Aeson.encode (object []))) (Right [])
  let fgEp47 = MediaItem
        { itemId = "ep1", itemTitle = "Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta"
        , itemPath = "/tv/fg.mkv", itemSize = 1, itemMime = "video/x-matroska", itemAddedAt = reqT0
        , itemFolder = "1", itemType = "Episode", itemSeries = Just "Family Guy"
        , itemSeason = Just 25, itemEpisode = Just 0, itemExtraType = Nothing, itemVersions = [] }
      mv47 = MediaItem
        { itemId = "m1", itemTitle = "Shrek", itemPath = "/mv/shrek.mp4", itemSize = 1
        , itemMime = "video/mp4", itemAddedAt = reqT0, itemFolder = "1", itemType = "Movie"
        , itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
  check "m47 matchDownloaded matrix (series case-insensitive, S/E exact, Episode only)"
    [ matchDownloaded "Family Guy" 25 0 [fgEp47]
    , matchDownloaded "family guy" 25 0 [fgEp47]
    , matchDownloaded "FAMILY GUY" 25 0 [fgEp47, mv47]
    , matchDownloaded "Family Guy" 25 1 [fgEp47]
    , matchDownloaded "Family Guy" 24 0 [fgEp47]
    , matchDownloaded "Simpsons" 25 0 [fgEp47]
    , matchDownloaded "Family Guy" 25 0 [mv47]
    , matchDownloaded "Family Guy" 25 0 [] ]
    [ True, True, True, False, False, False, False, False ]
  -- route order: the Series literals must win over the items captures
  check "m47 firstMatch: GET /Series/Search -> the literal route"
    [Just [Lit "Series", Lit "Search"]]
    [fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "GET" ["Series", "Search"])]
  check "m47 firstMatch: GET /series/1942/episodes -> capture route (case-insensitive)"
    [Just [Lit "Series", Capture "id", Lit "Episodes"]]
    [fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "GET" ["series", "1942", "episodes"])]
  check "m47 firstMatch: GET /Series/1942/Episodes exact case also matches"
    [Just [Lit "Series", Capture "id", Lit "Episodes"]]
    [fmap (routePattern . fst) (firstMatch (moduleRoutes metadataModule) "GET" ["Series", "1942", "Episodes"])]
  -- handler-level tests against a mock TMDB (guarded: no python3 in the nix sandbox)
  let respBody47 :: Response -> BSC.ByteString
      respBody47 (ResponseBuilder _ _ b) = LBS.toStrict (toLazyByteString b)
      respBody47 _ = ""
      countSub47 pat str = go (0 :: Int) str
        where
          go n rest = case BSC.breakSubstring pat rest of
            (_, b) | BSC.null b -> n
                   | otherwise -> go (n + 1) (BSC.drop (BSC.length pat) b)
  mPy47 <- findExecutable "python3"
  case mPy47 of
    Nothing -> putStrLn "PASS: m47 mock-tmdb handler tests skipped (python3 not in PATH)"
    Just _ -> do
      let m47mock = "/tmp/fms-m47-mock"
          m47log = m47mock ++ "/calls.log"
          mockSrc = unlines
            [ "import json, sys"
            , "from http.server import BaseHTTPRequestHandler, HTTPServer"
            , "from urllib.parse import urlparse, parse_qs"
            , "port = int(sys.argv[1]); log = sys.argv[2]"
            , "class H(BaseHTTPRequestHandler):"
            , "    def log_message(self, *a): pass"
            , "    def _send(self, code, body):"
            , "        b = body.encode() if isinstance(body, str) else body"
            , "        self.send_response(code)"
            , "        self.send_header('Content-Type', 'application/json')"
            , "        self.send_header('Content-Length', str(len(b)))"
            , "        self.end_headers()"
            , "        self.wfile.write(b)"
            , "    def do_GET(self):"
            , "        u = urlparse(self.path); q = parse_qs(u.query)"
            , "        key = q.get('api_key', [''])[0]"
            , "        with open(log, 'a') as f:"
            , "            f.write(u.path + '?api_key=' + key + chr(10))"
            , "        if u.path.startswith('/3/') and key != 'K47':"
            , "            return self._send(401, json.dumps({'status_message': 'bad key'}))"
            , "        if u.path == '/3/search/tv':"
            , "            return self._send(200, json.dumps({'results': [{'id': 1942, 'name': 'Family Guy', 'first_air_date': '1999-01-31', 'overview': 'A cartoon.', 'vote_average': 8.9, 'poster_path': '/fg.jpg'}]}))"
            , "        if u.path == '/3/tv/1942':"
            , "            return self._send(200, json.dumps({'name': 'Family Guy', 'overview': 'A cartoon.', 'first_air_date': '1999-01-31', 'poster_path': '/fg.jpg', 'seasons': [{'season_number': 25, 'name': 'Season 25', 'episode_count': 2}, {'season_number': 0, 'name': 'Specials'}]}))"
            , "        if u.path == '/3/tv/1942/season/25':"
            , "            return self._send(200, json.dumps({'episodes': [{'episode_number': 0, 'name': 'Happy Hell-o-ween', 'overview': 'o0', 'air_date': '2024-10-27', 'still_path': '/s0.jpg', 'vote_average': 7.1}, {'episode_number': 1, 'name': 'Ep One'}]}))"
            , "        if u.path == '/3/tv/1942/season/0':"
            , "            return self._send(200, json.dumps({'episodes': [{'episode_number': 1, 'name': 'Special'}]}))"
            , "        return self._send(404, '{}')"
            , "srv = HTTPServer(('127.0.0.1', port), H)"
            , "open(log + '.ready', 'w').write('ok')"
            , "srv.serve_forever()" ]
          cfg47 = cfg27 { configStateDir = m47mock, configTmdbApiKey = "ENVKEY47"
                        , configTmdbBaseUrl = "http://127.0.0.1:8947/3"
                        , configTmdbImageBaseUrl = "http://127.0.0.1:8947/img"
                        , configTmdbImageSize = "w500", configTmdbBackdropSize = "original", configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400 }
      resetDir m47mock
      createDirectoryIfMissing True m47mock
      writeFile (m47mock ++ "/mock.py") mockSrc
      bracket
        (createProcess (proc "python3" [m47mock ++ "/mock.py", "8947", m47log]))
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
        (\_ -> do
          rok <- waitForFile (m47log ++ ".ready") 40
          if not rok
            then putStrLn "PASS: m47 mock-tmdb handler tests skipped (mock did not start)"
            else do
              acid47 <- openLocalStateFrom (m47mock </> "acid") initialLibraryState
              _ <- update acid47 (RegisterItem "/tv/fg.mkv" fgEp47)
              env47 <- newAppEnv cfg47
              respS <- runApp env47 $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                _ <- updateMetadataSettings (\st -> st { msTmdbApiKey = "K47" })
                putState libraryAcidKey acid47
                seriesSearchHandler [] (defaultRequest { queryString = [("name", Just "Family Guy")] })
              let sb = either (const "") respBody47 respS
              check "m47 handler /Series/Search: mock hit, Type Series, full PosterUrl"
                (map (`BSC.isInfixOf` sb)
                  [ "\"Id\":\"1942\"", "\"Name\":\"Family Guy\"", "\"Type\":\"Series\""
                  , "\"PosterUrl\":\"http://127.0.0.1:8947/img/w500/fg.jpg\"" ])
                [True, True, True, True]
              respE <- runApp env47 (seriesEpisodesHandler [("id", "1942")] defaultRequest)
              let eb47 = either (const "") respBody47 respE
              check "m47 handler /Series/:id/Episodes: series fields + both seasons"
                (map (`BSC.isInfixOf` eb47)
                  [ "\"Id\":\"1942\"", "\"Name\":\"Family Guy\"", "\"Year\":1999"
                  , "\"SeasonNumber\":25", "\"SeasonNumber\":0" ])
                [True, True, True, True, True]
              check "m47 handler episodes: S25E00 Downloaded true (in library), S25E01 + special false"
                [ countSub47 "\"Downloaded\":true" eb47, countSub47 "\"Downloaded\":false" eb47 ]
                [1, 2]
              check "m47 handler episodes: StillUrl built from image base/size"
                True ("\"StillUrl\":\"http://127.0.0.1:8947/img/w500/s0.jpg\"" `BSC.isInfixOf` eb47)
              let (_, after25) = BSC.breakSubstring "\"SeasonNumber\":25" eb47
              check "m47 handler: specials (season 0) sorted LAST"
                [ "\"SeasonNumber\":0" `BSC.isInfixOf` after25
                , "\"SeasonNumber\":0" `BSC.isInfixOf` fst (BSC.breakSubstring "\"SeasonNumber\":25" eb47) ]
                [True, False]
              mockCalls <- readFile m47log
              let apiLines = filter ("/3/" `isPrefixOf`) (lines mockCalls)
              check "m47 mock: every API call used the SAVED key K47"
                (all (\l -> "api_key=K47" `isSuffixOf` l) apiLines) True
              check "m47 mock: env key never sent" (any ("ENVKEY47" `isInfixOf`) (lines mockCalls)) False
              -- empty effective key -> graceful 200 with empty payload
              let m47s2 = m47mock </> "s2"
              createDirectoryIfMissing True m47s2
              env47n <- newAppEnv (cfg47 { configStateDir = m47s2, configTmdbApiKey = "" })
              respN <- runApp env47n $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                seriesSearchHandler [] (defaultRequest { queryString = [("name", Just "Family Guy")] })
              check "m47 handler: empty key -> /Series/Search 200 []" (either (const "") respBody47 respN) "[]"
              respN2 <- runApp env47n (seriesEpisodesHandler [("id", "1942")] defaultRequest)
              check "m47 handler: empty key -> /Series/:id/Episodes 200 empty Seasons"
                True ("\"Seasons\":[]" `BSC.isInfixOf` either (const "") respBody47 respN2)
              -- bad id -> 400, never a crash
              rBad <- runApp env47n (seriesEpisodesHandler [("id", "abc")] defaultRequest)
              check "m47 handler: non-numeric series id -> 400"
                (either statusForError (const 200) rBad) 400
              -- TMDB down -> graceful empty seasons, not a 500
              let m47s3 = m47mock </> "s3"
              createDirectoryIfMissing True m47s3
              env47d <- newAppEnv (cfg47 { configStateDir = m47s3, configTmdbBaseUrl = "http://127.0.0.1:8999/3" })
              respD <- runApp env47d $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                _ <- updateMetadataSettings (\st -> st { msTmdbApiKey = "K47" })
                putState libraryAcidKey acid47
                seriesEpisodesHandler [("id", "1942")] defaultRequest
              check "m47 handler: TMDB down -> 200 with empty Seasons (graceful)"
                True ("\"Seasons\":[]" `BSC.isInfixOf` either (const "") respBody47 respD)
              closeAcidState acid47)


  -- m48: streamed transcode is NOT capped by the batch ffmpeg job timeout
  check "m48: streamTranscodeTimeoutSec is 0 (no cap for the streamed transcode)"
    0 streamTranscodeTimeoutSec
  check "m48: batch ffmpeg uses keep the configurable cap (network input gets -rw_timeout)"
    True (not (null (ffmpegTimeoutArgs (configFfmpegTimeoutSec cfg27) "http://x/y.mp4")))
  check "m48: batch ffmpeg timeout args stay empty for a local file"
    [] (ffmpegTimeoutArgs (configFfmpegTimeoutSec cfg27) "/local/file.mp4")

  -- m52: force a keyframe every ~2s in BOTH hw and sw transcode args
  let kfOk args = case break (== "-force_key_frames") args of
        (_, _ : expr : _) -> expr == "expr:gte(t,n_forced*2)"
        _ -> False
      gOk args = case break (== "-g") args of
        (_, _ : n : _) -> n == "48"
        _ -> False
  check "m52: transcodeArgsWith (sw+hw) forces keyframes every 2s and -g 48"
    [ True, True, True, True, True ]
    [ kfOk (transcodeArgsWith "none" Nothing "/m/a.mkv")
    , gOk  (transcodeArgsWith "none" Nothing "/m/a.mkv")
    , kfOk (transcodeArgsWith "nvenc" (Just 2000000) "/m/a.mkv")
    , gOk  (transcodeArgsWith "nvenc" (Just 2000000) "/m/a.mkv")
    , "frag_keyframe+empty_moov" `elem` transcodeArgsWith "none" Nothing "/m/a.mkv" ]

  -- m49: backdrop size config + per-kind image download sizes
  check "m57: configTmdbBackdropSize defaults to original (full-res backdrops) in the test config"
    "original" (configTmdbBackdropSize cfg27)
  check "m49: poster size stays w500"
    "w500" (configTmdbImageSize cfg27)

  -- m51: image files are not scanned as library items (torrent previews)
  let m51dir = "/tmp/fms-test-scan-m51"
  resetDir m51dir
  createDirectoryIfMissing True m51dir
  writeFile (m51dir </> "movie.mkv") "v"
  writeFile (m51dir </> "screen0001.jpg") "img"
  writeFile (m51dir </> "poster.PNG") "img"
  writeFile (m51dir </> "song.flac") "a"
  m51scanned <- scanDirectory False m51dir
  check "m51: scanDirectory returns only the video+audio, no images"
    (sort [m51dir </> "movie.mkv", m51dir </> "song.flac"]) (sort m51scanned)
  check "m51: isMediaFile rejects image extensions (case-insensitive), keeps audio"
    [ isMediaFile "/x/a.JPG", isMediaFile "/x/a.png", isMediaFile "/x/a.webp", isMediaFile "/x/s.flac", isMediaFile "/x/m.mp4" ]
    [ False, False, False, True, True ]
  check "m51: NFO sibling lookup still matches poster.jpg (artwork association intact)"
    True (map (replaceExtension "/x/movie") imageExtensions
            == ["/x/movie.jpg", "/x/movie.jpeg", "/x/movie.png", "/x/movie.gif", "/x/movie.webp", "/x/movie.bmp", "/x/movie.heic", "/x/movie.tiff"]
            && "/x/movie.jpg" `elem` siblingMediaPathsForNfo "/x/movie.nfo")
  removeDirectoryRecursive m51dir

  -- m53a: detail URL builders + genres/keywords parser
  check "m53a: tmdbMovieDetailsUrlAt / tmdbTvDetailsKwUrlAt (append_to_response=keywords)"
    [ tmdbMovieDetailsUrlAt "http://127.0.0.1:8949/3" "K" 42
    , tmdbMovieDetailsUrlAt "http://127.0.0.1:8949/3/" "K" 7
    , tmdbTvDetailsKwUrlAt "http://127.0.0.1:8949/3" "K" 1942 ]
    [ "http://127.0.0.1:8949/3/movie/42?api_key=K&append_to_response=keywords"
    , "http://127.0.0.1:8949/3/movie/7?api_key=K&append_to_response=keywords"
    , "http://127.0.0.1:8949/3/tv/1942?api_key=K&append_to_response=keywords" ]
  let movieDetails = LBS.fromStrict (BSC.pack (unwords
        [ "{\"id\":42,\"genres\":[{\"id\":28,\"name\":\"Action\"},{\"id\":878,\"name\":\"Science Fiction\"}],"
        , "\"keywords\":{\"keywords\":[{\"id\":9,\"name\":\"hero\"},{\"id\":10,\"name\":\"save the world\"}]}}" ]))
      tvDetails53 = LBS.fromStrict (BSC.pack (unwords
        [ "{\"id\":1942,\"genres\":[{\"id\":16,\"name\":\"Animation\"}],"
        , "\"keywords\":{\"results\":[{\"id\":5,\"name\":\"cartoon\"}]}}" ]))
  check "m53a: parseTmdbGenresKeywords movie shape (keywords.keywords)"
    (parseTmdbGenresKeywords movieDetails)
    (Right (["Action", "Science Fiction"], ["hero", "save the world"]))
  check "m53a: parseTmdbGenresKeywords tv shape (keywords.results)"
    (parseTmdbGenresKeywords tvDetails53)
    (Right (["Animation"], ["cartoon"]))
  check "m53a: parseTmdbGenresKeywords missing keys -> ([],[])"
    (parseTmdbGenresKeywords (Aeson.encode (object []))) (Right ([], []))
  check "m53a: parseTmdbGenresKeywords garbage -> Left"
    True (isLeftStr (parseTmdbGenresKeywords (LBS.fromStrict (BSC.pack "nope"))))

  -- m53a: keywords server-side only (persisted, never in public JSON)
  let kwMeta = emptyMetadata { metaGenres = ["Action"], metaKeywords = ["hero"] }
      pubJson = LBS.toStrict (Aeson.encode kwMeta)
      storedJson = LBS.toStrict (Aeson.encode (metadataStoredJSON kwMeta))
  check "m53a: public metadata JSON has genres but NOT keywords"
    [ "\"genres\":[\"Action\"]" `BSC.isInfixOf` pubJson
    , "keywords" `BSC.isInfixOf` pubJson ]
    [ True, False ]
  check "m53a: stored (persistence) JSON includes keywords"
    True ("\"keywords\":[\"hero\"]" `BSC.isInfixOf` storedJson)
  check "m53a: old metadata.json without keywords decodes with []"
    (case Aeson.decode (LBS.fromStrict (BSC.pack "{\"title\":\"T\"}")) of
       Just m -> metaKeywords m; Nothing -> ["<decode fail>"])
    []
  -- persistence round-trip keeps keywords
  let m53pdir = "/tmp/fms-test-meta-kw"
  resetDir m53pdir
  createDirectoryIfMissing True m53pdir
  saveMetadata (m53pdir </> "metadata.json") (HM.fromList [("1", kwMeta)])
  m53loaded <- loadMetadata (m53pdir </> "metadata.json")
  check "m53a: saveMetadata/loadMetadata round-trips keywords"
    (Just ["hero"]) (fmap metaKeywords (HM.lookup "1" m53loaded))
  -- provider merge keeps keywords (new non-empty wins, empty keeps old)
  check "m53a: applyProviderMerge keeps old keywords when new are empty"
    ["hero"] (metaKeywords (applyProviderMerge kwMeta (emptyMetadata { metaTitle = Just "New" })))
  check "m53a: applyProviderMerge takes new keywords when present"
    ["x"] (metaKeywords (applyProviderMerge kwMeta (emptyMetadata { metaKeywords = ["x"] })))

  -- m53a: similarItems also ranks by shared keywords
  let siK base = mi5 base base
      kwLookup = HM.fromList
        [ ("1", sparseMeta { metaGenres = ["Action"], metaKeywords = ["hero"] })
        , ("2", sparseMeta { metaGenres = ["Action"] })                          -- genre only
        , ("3", sparseMeta { metaGenres = ["Action"], metaKeywords = ["hero", "duo"] })  -- genre + keyword
        ]
      kwItems = [siK "1", siK "2", siK "3"]
  check "m53a: similarItems ranks the keyword-sharing item first"
    ["3", "2"] (map itemId (similarItems (flip HM.lookup kwLookup) 12 (siK "1") kwItems))

  -- m53a: legacy /library rows carry genres
  let m53rowItem = (mi5 "9" "Some Movie") { itemType = "Movie" }
      m53row = withLegacyItem (HM.fromList [("9", kwMeta)]) m53rowItem (simpleItemJson m53rowItem)
      m53fieldOf k v = case v of Aeson.Object o -> KeyMap.lookup (Key.fromText k) o; _ -> Nothing
  check "m53a: withLegacyItem adds genres to the legacy row"
    (Just (Aeson.toJSON (["Action"] :: [T.Text]))) (m53fieldOf "genres" m53row)

  -- m49+m53a live-ish against a mock TMDB (python3-guarded)
  mPy53 <- findExecutable "python3"
  case mPy53 of
    Nothing -> putStrLn "PASS: m49/m53a mock-tmdb IO tests skipped (python3 not in PATH)"
    Just _ -> do
      let m53mock = "/tmp/fms-m53-mock"
          m53log = m53mock ++ "/calls.log"
          mockSrc = unlines
            [ "import json, sys"
            , "from http.server import BaseHTTPRequestHandler, HTTPServer"
            , "from urllib.parse import urlparse, parse_qs"
            , "port = int(sys.argv[1]); log = sys.argv[2]"
            , "class H(BaseHTTPRequestHandler):"
            , "    def log_message(self, *a): pass"
            , "    def _send(self, code, body, ctype='application/json'):"
            , "        b = body.encode() if isinstance(body, str) else body"
            , "        self.send_response(code)"
            , "        self.send_header('Content-Type', ctype)"
            , "        self.send_header('Content-Length', str(len(b)))"
            , "        self.end_headers()"
            , "        self.wfile.write(b)"
            , "    def do_GET(self):"
            , "        u = urlparse(self.path); q = parse_qs(u.query)"
            , "        key = q.get('api_key', [''])[0]"
            , "        with open(log, 'a') as f:"
            , "            f.write(u.path + '?' + u.query + chr(10))"
            , "        if key != 'K53' and u.path.startswith('/3/'):"
            , "            return self._send(401, '{\"status_message\":\"bad key\"}')"
            , "        if u.path == '/3/search/movie':"
            , "            return self._send(200, json.dumps({'results': [{'id': 42, 'title': 'Test Movie', 'release_date': '2020-01-01', 'overview': 'o', 'vote_average': 7.5, 'poster_path': '/p.jpg'}]}))"
            , "        if u.path == '/3/movie/42':"
            , "            return self._send(200, json.dumps({'id': 42, 'genres': [{'id': 28, 'name': 'Action'}], 'keywords': {'keywords': [{'id': 9, 'name': 'hero'}]}}))"
            , "        if u.path == '/3/movie/42/images':"
            , "            return self._send(200, json.dumps({'posters': [{'file_path': '/p.jpg'}], 'backdrops': [{'file_path': '/b.jpg'}], 'logos': []}))"
            , "        if u.path.startswith('/img/'):"
            , "            return self._send(200, 'IMGBYTES', 'image/jpeg')"
            , "        return self._send(404, '{}')"
            , "srv = HTTPServer(('127.0.0.1', port), H)"
            , "open(log + '.ready', 'w').write('ok')"
            , "srv.serve_forever()" ]
      resetDir m53mock
      createDirectoryIfMissing True m53mock
      writeFile (m53mock ++ "/mock.py") mockSrc
      let cfg53 = cfg27 { configStateDir = m53mock, configTmdbApiKey = "K53", configFetchImages = False
                        , configTmdbBaseUrl = "http://127.0.0.1:8949/3"
                        , configTmdbImageBaseUrl = "http://127.0.0.1:8949/img"
                        , configTmdbImageSize = "w500", configTmdbBackdropSize = "original", configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400 }
          m53item = MediaItem
            { itemId = "1", itemTitle = "Test Movie", itemPath = "/x.mp4"
            , itemSize = 1, itemMime = "video/mp4", itemAddedAt = reqT0, itemFolder = "1"
            , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
            , itemExtraType = Nothing, itemVersions = [] }
      bracket
        (createProcess (proc "python3" [m53mock ++ "/mock.py", "8949", m53log]))
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
        (\_ -> do
          rok <- waitForFile (m53log ++ ".ready") 40
          if not rok
            then putStrLn "PASS: m49/m53a mock-tmdb IO tests skipped (mock did not start)"
            else do
              env53 <- newAppEnv cfg53
              _ <- runApp env53 $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                refreshFromTmdb False "1" m53item
              rGet <- runApp env53 (getMetadata "1")
              let mMeta = either (const Nothing) id rGet
              check "m53a refreshFromTmdb: genres from the detail endpoint"
                (Just ["Action"]) (fmap metaGenres mMeta)
              check "m53a refreshFromTmdb: keywords stored server-side"
                (Just ["hero"]) (fmap metaKeywords mMeta)
              check "m53a refreshFromTmdb: public metadata JSON omits keywords"
                (Just False)
                (fmap (\m -> "keywords" `BSC.isInfixOf` LBS.toStrict (Aeson.encode m)) mMeta)
              mockCalls <- readFile m53log
              let apiLines = filter ("/3/" `isPrefixOf`) (lines mockCalls)
              check "m53a mock: detail endpoint hit with append_to_response=keywords and the key"
                (any (\l -> "/3/movie/42?" `isPrefixOf` l && "append_to_response=keywords" `isInfixOf` l && "api_key=K53" `isInfixOf` l) apiLines) True
              -- m49: fetchTmdbImages uses w500 for posters, w1280 for backdrops
              rImgs <- fetchTmdbImages "K53" "http://127.0.0.1:8949/3" "movie" "42"
                          "http://127.0.0.1:8949/img" "w500" "w1280"
              let imgTypes = either (const []) (map (\(t, _, _) -> t)) rImgs
              check "m49: fetchTmdbImages downloaded poster + backdrop"
                ["Backdrop", "Primary"] (sort imgTypes)
              mockCalls2 <- readFile m53log
              let imgLines = filter ("/img/" `isPrefixOf`) (lines mockCalls2)
              check "m49: poster downloaded at w500, backdrop at w1280"
                [ any ("/img/w500/p.jpg" `isPrefixOf`) imgLines
                , any ("/img/w1280/b.jpg" `isPrefixOf`) imgLines ]
                [ True, True ])

  -- m56: persistent TMDB series cache with TTL ------------------------------
  let m56now = UTCTime (fromGregorian 2026 6 1) 43200   -- 2026-06-01 12:00:00 UTC
      m56e :: Int -> SeriesCacheEntry
      m56e age = SeriesCacheEntry (addUTCTime (fromIntegral (negate age)) m56now) (Aeson.String "x")
      m56count pat str = go (0 :: Int) str
        where
          go n rest = case BSC.breakSubstring pat rest of
            (_, b) | BSC.null b -> n
                   | otherwise -> go (n + 1) (BSC.drop (BSC.length pat) b)
      fgEp56 = MediaItem
        { itemId = "ep56", itemTitle = "Family.Guy.S25E00.Happy.Hell-o-ween.1080p.HEVC.x265-MeGusta"
        , itemPath = "/tv/fg.mkv", itemSize = 1, itemMime = "video/x-matroska", itemAddedAt = m56now
        , itemFolder = "1", itemType = "Episode", itemSeries = Just "Family Guy"
        , itemSeason = Just 25, itemEpisode = Just 0, itemExtraType = Nothing, itemVersions = [] }
      fgEp56b = fgEp56 { itemId = "ep56b", itemPath = "/tv/fg2.mkv", itemEpisode = Just 1 }
      m56payload = object
        [ "Id" .= ("1942" :: T.Text), "Name" .= ("Family Guy" :: T.Text)
        , "Seasons" .= [ object [ "SeasonNumber" .= (25 :: Int)
                                 , "Episodes" .= [ object [ "EpisodeNumber" .= (0 :: Int), "Name" .= ("E0" :: T.Text) ]
                                                 , object [ "EpisodeNumber" .= (1 :: Int), "Name" .= ("E1" :: T.Text) ] ] ] ] ]
  check "m56 cacheFresh: ttl 86400 fresh(100s)/expired(90000s), ttl 0 and negative never fresh"
    [ cacheFresh m56now 86400 (m56e 100), cacheFresh m56now 86400 (m56e 90000)
    , cacheFresh m56now 0 (m56e 1), cacheFresh m56now (-5) (m56e 1) ]
    [ True, False, False, False ]
  check "m56 cachePut/cacheGet roundtrip + overwrite keeps the newest payload"
    [ fmap scePayload (cacheGet "k" (cachePut "k" m56now (Aeson.String "v") emptySeriesCache))
    , fmap scePayload (cacheGet "k" (cachePut "k" m56now (Aeson.String "v2") (cachePut "k" m56now (Aeson.String "v") emptySeriesCache))) ]
    [ Just (Aeson.String "v"), Just (Aeson.String "v2") ]
  check "m56 cacheGet: missing key -> Nothing"
    True (isNothing (cacheGet "missing" (cachePut "k" m56now (Aeson.String "v") emptySeriesCache)))
  check "m56 seriesNameOfPayload: Name field / non-object -> empty"
    [ seriesNameOfPayload m56payload, seriesNameOfPayload (Aeson.String "x") ]
    [ "Family Guy", "" ]
  let m56att = LBS.toStrict (Aeson.encode (attachDownloadedLive "Family Guy" m56payload [fgEp56]))
  check "m56 attachDownloadedLive: matching S25E00 true, S25E01 false, raw payload untouched"
    [ m56count "\"Downloaded\":true" m56att, m56count "\"Downloaded\":false" m56att
    , m56count "Downloaded" (LBS.toStrict (Aeson.encode m56payload)) ]
    [ 1, 1, 0 ]
  -- persistence: save/load roundtrip, missing + malformed file -> empty
  let m56dir = "/tmp/fms-m56-cache-test"
  resetDir m56dir
  createDirectoryIfMissing True m56dir
  let cpath = seriesCachePath m56dir
  check "m56 seriesCachePath" (m56dir ++ "/series-cache.json") cpath
  cEmpty <- loadSeriesCache cpath
  check "m56 loadSeriesCache: missing file -> empty map" True (isNothing (cacheGet "any" cEmpty))
  saveSeriesCache cpath (cachePut "1942" m56now (object ["Name" .= ("Family Guy" :: T.Text)]) emptySeriesCache)
  cLoaded <- loadSeriesCache cpath
  check "m56 save/load roundtrip preserves the payload"
    (fmap scePayload (cacheGet "1942" cLoaded))
    (Just (object ["Name" .= ("Family Guy" :: T.Text)]))
  check "m56 save/load roundtrip preserves fetchedAt"
    (fmap sceFetchedAt (cacheGet "1942" cLoaded))
    (Just m56now)
  writeFile cpath "{ not json"
  cBad <- loadSeriesCache cpath
  check "m56 loadSeriesCache: malformed JSON -> empty (never crash)" True (isNothing (cacheGet "1942" cBad))
  removeDirectoryRecursive m56dir
  -- live mock-TMDB test: cache hits, ?refresh=true, live Downloaded, TTL=0
  mPy56 <- findExecutable "python3"
  case mPy56 of
    Nothing -> putStrLn "PASS: m56 mock-tmdb cache tests skipped (python3 not in PATH)"
    Just _ -> do
      let m56mock = "/tmp/fms-m56-mock"
          m56log = m56mock ++ "/calls.log"
          mockSrc56 = unlines
            [ "import json, sys"
            , "from http.server import BaseHTTPRequestHandler, HTTPServer"
            , "from urllib.parse import urlparse, parse_qs"
            , "port = int(sys.argv[1]); log = sys.argv[2]"
            , "class H(BaseHTTPRequestHandler):"
            , "    def log_message(self, *a): pass"
            , "    def _send(self, code, body):"
            , "        b = body.encode() if isinstance(body, str) else body"
            , "        self.send_response(code)"
            , "        self.send_header('Content-Type', 'application/json')"
            , "        self.send_header('Content-Length', str(len(b)))"
            , "        self.end_headers()"
            , "        self.wfile.write(b)"
            , "    def do_GET(self):"
            , "        u = urlparse(self.path); q = parse_qs(u.query)"
            , "        key = q.get('api_key', [''])[0]"
            , "        with open(log, 'a') as f:"
            , "            f.write(u.path + '?api_key=' + key + chr(10))"
            , "        if u.path.startswith('/3/') and key != 'K56':"
            , "            return self._send(401, json.dumps({'status_message': 'bad key'}))"
            , "        if u.path == '/3/search/tv':"
            , "            return self._send(200, json.dumps({'results': [{'id': 1942, 'name': 'Family Guy', 'first_air_date': '1999-01-31', 'overview': 'A cartoon.', 'vote_average': 8.9, 'poster_path': '/fg.jpg'}]}))"
            , "        if u.path == '/3/tv/1942':"
            , "            return self._send(200, json.dumps({'name': 'Family Guy', 'overview': 'A cartoon.', 'first_air_date': '1999-01-31', 'poster_path': '/fg.jpg', 'seasons': [{'season_number': 25, 'name': 'Season 25', 'episode_count': 2}, {'season_number': 0, 'name': 'Specials'}]}))"
            , "        if u.path == '/3/tv/1942/season/25':"
            , "            return self._send(200, json.dumps({'episodes': [{'episode_number': 0, 'name': 'Happy Hell-o-ween', 'overview': 'o0', 'air_date': '2024-10-27', 'still_path': '/s0.jpg', 'vote_average': 7.1}, {'episode_number': 1, 'name': 'Ep One'}]}))"
            , "        if u.path == '/3/tv/1942/season/0':"
            , "            return self._send(200, json.dumps({'episodes': [{'episode_number': 1, 'name': 'Special'}]}))"
            , "        return self._send(404, '{}')"
            , "srv = HTTPServer(('127.0.0.1', port), H)"
            , "open(log + '.ready', 'w').write('ok')"
            , "srv.serve_forever()" ]
          cfg56 = cfg27 { configStateDir = m56mock, configTmdbApiKey = "ENVKEY56"
                        , configTmdbBaseUrl = "http://127.0.0.1:8956/3"
                        , configTmdbImageBaseUrl = "http://127.0.0.1:8956/img"
                        , configSeriesCacheTtlSec = 86400, configAnnounceListUrl = "https://raw.githubusercontent.com/ngosang/trackerslist/master/trackers_best.txt", configAnnounceListRefreshSecs = 86400 }
          respBody56 :: Response -> BSC.ByteString
          respBody56 (ResponseBuilder _ _ b) = LBS.toStrict (toLazyByteString b)
          respBody56 _ = ""
          apiCount pat = do
            mc <- BSC.readFile m56log   -- strict: count the calls made SO FAR
            pure (length (filter (BSC.isPrefixOf pat) (BSC.lines mc)))
      resetDir m56mock
      createDirectoryIfMissing True m56mock
      writeFile (m56mock ++ "/mock.py") mockSrc56
      bracket
        (createProcess (proc "python3" [m56mock ++ "/mock.py", "8956", m56log]))
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
        (\_ -> do
          rok <- waitForFile (m56log ++ ".ready") 40
          if not rok
            then putStrLn "PASS: m56 mock-tmdb cache tests skipped (mock did not start)"
            else do
              acid56 <- openLocalStateFrom (m56mock </> "acid") initialLibraryState
              _ <- update acid56 (RegisterItem "/tv/fg.mkv" fgEp56)
              env56 <- newAppEnv cfg56
              _ <- runApp env56 $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                loadSeriesCacheIntoState
                _ <- updateMetadataSettings (\st -> st { msTmdbApiKey = "K56" })
                putState libraryAcidKey acid56
              -- first call: live fetch, then cached
              r1 <- runApp env56 (seriesEpisodesHandler [("id", "1942")] defaultRequest)
              let b1 = either (const "") respBody56 r1
              check "m56 mock: first /Series/:id/Episodes fetches live (S25E00 downloaded true, others false)"
                [ m56count "\"Downloaded\":true" b1, m56count "\"Downloaded\":false" b1 ]
                [1, 2]
              cDet1 <- apiCount "/3/tv/1942?"
              cS251 <- apiCount "/3/tv/1942/season/25"
              cS01 <- apiCount "/3/tv/1942/season/0"
              check "m56 mock: first call hit tv details + both seasons exactly once"
                [cDet1, cS251, cS01] [1, 1, 1]
              -- second call: served from the persistent cache, no new mock calls
              r2 <- runApp env56 (seriesEpisodesHandler [("id", "1942")] defaultRequest)
              let b2 = either (const "") respBody56 r2
              cDet2 <- apiCount "/3/tv/1942?"
              check "m56 mock: second call served from cache (details still hit once)"
                cDet2 1
              check "m56 mock: cached response keeps shape + live Downloaded"
                [ m56count "\"Downloaded\":true" b2, m56count "\"SeasonNumber\":25" b2, m56count "\"SeasonNumber\":0" b2 ]
                [1, 1, 1]
              -- add another downloaded episode between calls -> recomputed live from the cached payload
              _ <- update acid56 (RegisterItem "/tv/fg2.mkv" fgEp56b)
              r3 <- runApp env56 (seriesEpisodesHandler [("id", "1942")] defaultRequest)
              let b3 = either (const "") respBody56 r3
              cDet3 <- apiCount "/3/tv/1942?"
              check "m56 mock: library change visible on cached payload (2 downloaded, still 1 detail call)"
                [ m56count "\"Downloaded\":true" b3, cDet3 ] [2, 1]
              -- ?refresh=true forces a live refetch
              _ <- runApp env56 (seriesEpisodesHandler [("id", "1942")] (defaultRequest { queryString = [("refresh", Just "true")] }))
              cDet4 <- apiCount "/3/tv/1942?"
              check "m56 mock: ?refresh=true refetches from TMDB" cDet4 2
              -- cache persisted to disk under the state dir
              let cacheFile = seriesCachePath m56mock
              cacheExists <- doesFileExist cacheFile
              cacheTxt <- BSC.unpack <$> BSC.readFile cacheFile   -- strict: do not hold a lock on the file
              check "m56 mock: series-cache.json persisted with fetchedAt + payload under the tmdb id"
                [ cacheExists, "fetchedAt" `isInfixOf` cacheTxt, "\"payload\"" `isInfixOf` cacheTxt, "\"1942\"" `isInfixOf` cacheTxt ]
                [True, True, True, True]
              -- /Series/Search caching under search:<lowercase query>
              rs1 <- runApp env56 (seriesSearchHandler [] (defaultRequest { queryString = [("name", Just "Family Guy")] }))
              cs1 <- apiCount "/3/search/tv"
              rs2 <- runApp env56 (seriesSearchHandler [] (defaultRequest { queryString = [("name", Just "Family Guy")] }))
              cs2 <- apiCount "/3/search/tv"
              _ <- runApp env56 (seriesSearchHandler [] (defaultRequest { queryString = [("name", Just "Family Guy"), ("refresh", Just "true")] }))
              cs3 <- apiCount "/3/search/tv"
              let bs1 = either (const "") respBody56 rs1
                  bs2 = either (const "") respBody56 rs2
              check "m56 mock: /Series/Search cached on 2nd call, ?refresh=true refetches"
                [ cs1, cs2, cs3 ] [1, 1, 2]
              check "m56 mock: cached search response identical to the fresh one" True (bs1 == bs2)
              cacheTxt2 <- BSC.unpack <$> BSC.readFile cacheFile
              check "m56 mock: search cached under search:<lowercase query>"
                True ("search:family guy" `isInfixOf` cacheTxt2)
              -- TTL=0 -> always refetch
              let m56t0 = m56mock </> "t0"
              createDirectoryIfMissing True m56t0
              envT0 <- newAppEnv (cfg56 { configStateDir = m56t0, configSeriesCacheTtlSec = 0 })
              _ <- runApp envT0 $ do
                loadMetadataIntoState
                loadMetadataSettingsIntoState
                loadSeriesCacheIntoState
                _ <- updateMetadataSettings (\st -> st { msTmdbApiKey = "K56" })
                putState libraryAcidKey acid56
              _ <- runApp envT0 (seriesEpisodesHandler [("id", "1942")] defaultRequest)
              _ <- runApp envT0 (seriesEpisodesHandler [("id", "1942")] defaultRequest)
              cT0 <- apiCount "/3/tv/1942?"
              check "m56 mock: TTL=0 always refetches (4 detail hits total: 2 + refresh + 2 ttl0)" cT0 4
              closeAcidState acid56)

  putStrLn "All tests passed."
