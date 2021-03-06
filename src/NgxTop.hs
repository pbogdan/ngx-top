{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NgxTop
  ( Settings(..)
  , defaultSettings
  , defaultMain
  , run
  ) where

import           Protolude hiding ((&), try)

import           Brick.BChan
import qualified Brick.Main
import           Control.Concurrent.STM.TVar
import           Control.Exception.Safe
import qualified Control.Foldl as Fold
import           Control.Lens hiding (lined)
import qualified Data.ByteString as Bytes
import           Data.GeoIP2
import qualified Data.HashMap.Strict as HashMap
import           Data.IP
import qualified Data.IntMap.Strict as IntMap
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           GeoIP
import           Graphics.Vty
import           Log.Nginx.Combined
import           Log.Nginx.Detect
import           Log.Nginx.Gateway
import           Log.Nginx.Types
import           NgxTop.Bots hiding (bots)
import           NgxTop.UI
import           Pipes hiding (for)
import qualified Pipes.ByteString as PB
import qualified Pipes.Group as PG
import           System.IO.TailFile.Pipes
import           Types
import           URI.ByteString

{-# ANN Settings ("HLint: ignore Use newtype instead of data" :: Text) #-}
data Settings = Settings
  { parsers :: NonEmpty (ByteString -> Either Text AccessLogEntry)
  }

defaultSettings :: Settings
defaultSettings = Settings {parsers = NE.fromList [parseGateway, parseCombined]}

defaultMain :: Settings -> IO ()
defaultMain settings = do
  args <- getArgs
  case args of
    [] -> do
      putText "Usage: ngx-top logfile [logfile...]"
      exitFailure
    xs -> run settings (NE.fromList xs)

run :: Settings -> NonEmpty FilePath -> IO ()
run Settings {..} paths = do
  line <-
    try
      (evaluate =<<
       (do h <- openFile (NE.head paths) ReadMode
           Bytes.hGetLine h))
  let parser =
        case detect (NE.toList parsers) <$> line of
          Left (_ :: SomeException) -> NE.head parsers
          Right (Just x) -> x
          _ -> parseGateway
  let
    Right db = openGeoDBBS geoIPDB
    initialStats =
        Stats
          0
          0
          IntMap.empty
          HashMap.empty
          HashMap.empty
          HashMap.empty
          0
          0
          HashMap.empty
          0
          db
          HashMap.empty
          HashMap.empty
  stats <- atomically $ newTVar initialStats
  eventChan <- newBChan 10
  as <-
    for paths $ \path -> async $ void $ tailFile path (updateStats stats parser)
  b <-
    async $
    void $ do
      let buildVty = mkVty defaultConfig {termName = Nothing}
      initialVty <- buildVty
      _ <-
        Brick.Main.customMain
          initialVty
          buildVty
          (Just eventChan)
          (app paths)
          initialStats
      return ()
  c <-
    async $
    void $
    forever $ do
      current <- atomically $ readTVar stats
      if current ^. totalRequests > 0
        then do
          atomically . modifyTVar' stats $ updateCount +~ 1
          writeBChan eventChan $ Update current
          threadDelay 1000000
        else threadDelay 5000
  (_, ret) <- waitAnyCatchCancel (NE.toList as ++ [b, c])
  case ret of
    Left e -> do
      putText . toS . displayException $ e
      exitFailure
    Right _ -> return ()

lined
  :: Monad m
  => Producer ByteString m x -> Producer ByteString m x
lined p = Fold.purely PG.folds Fold.mconcat (view PB.lines p)

parseLine
  :: Monad m
  => (ByteString -> Either Text AccessLogEntry)
  -> Pipes.Proxy () ByteString () AccessLogEntry m b
parseLine parser =
  forever $ do
    x <- await
    case parser x of
      Right l -> yield l
      Left _ -> return ()

updateStats
  :: MonadIO m
  => TVar Stats
  -> (ByteString -> Either Text AccessLogEntry)
  -> Producer ByteString m r
  -> m (void, r)
updateStats stats parser p = do
  _ <-
    runEffect $
    (p & lined) >-> parseLine parser >->
    forever
      (do x <- await
          liftIO . atomically $
            modifyTVar' stats $ \current ->
              let newBotsCache =
                    case HashMap.lookup (aleUserAgent x) (current ^. botsCache) of
                      Nothing ->
                        HashMap.insert
                          (aleUserAgent x)
                          (isBot (aleUserAgent x))
                          (current ^. botsCache)
                      Just _ -> current ^. botsCache
              in current & cacheHitCount .~
                 (if aleCacheStatus x == HIT
                    then current ^. cacheHitCount <> 1
                    else current ^. cacheHitCount) &
                 cacheMissCount .~
                 (if aleCacheStatus x /= HIT
                    then current ^. cacheMissCount <> 1
                    else current ^. cacheMissCount) &
                 responseCodes .~
                 IntMap.insertWith
                   (<>)
                   (aleResponseCode x)
                   1
                   (current ^. responseCodes) &
                 domains .~
                 HashMap.insertWith (<>) (aleHost x) 1 (current ^. domains) &
                 urls .~
                 HashMap.insertWith
                   (<>)
                   (aleHost x <> rrPath (requestUri $ aleRequest x))
                   1
                   (current ^. urls) &
                 responseTimes .~
                 (if aleCacheStatus x == HIT
                    then current ^. responseTimes
                    else HashMap.insertWith
                           (\(a, b) (c, d) -> (a + c, b + d))
                           (aleHost x <> rrPath (requestUri $ aleRequest x))
                           (1, aleResponseTime x)
                           (current ^. responseTimes)) &
                 responseTime .~
                 (if aleCacheStatus x == HIT
                    then current ^. responseTime
                    else current ^. responseTime <> (Sum . aleResponseTime $ x)) &
                 totalBandwidth .~
                 current ^.
                 totalBandwidth <>
                 Sum (aleResponseSize x) &
                 ips .~
                 HashMap.insertWith
                   (<>)
                   (fromIPv4 . aleIP $ x)
                   1
                   (current ^. ips) &
                 botsCache .~
                 newBotsCache &
                 bots .~
                 (case join (HashMap.lookup (aleUserAgent x) newBotsCache) of
                    Just bot -> HashMap.insertWith (<>) bot 1 (current ^. bots)
                    _ -> current ^. bots))
  return (undefined :: void, undefined :: r)
