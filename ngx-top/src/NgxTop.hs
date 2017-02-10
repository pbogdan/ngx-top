{-# LANGUAGE ScopedTypeVariables #-}

module NgxTop
  ( run
  ) where

import           Protolude hiding ((&))

import           Brick.BChan
import           Brick.Main
import           Control.Concurrent.STM.TVar
import qualified Control.Foldl as Fold
import           Control.Lens hiding (lined)
import           Data.GeoIP2
import qualified Data.HashMap.Strict as HashMap
import           Data.IP
import qualified Data.IntMap.Strict as IntMap
import           GeoIP
import           Graphics.Vty
import           Log.Nginx.Gateway
import           Log.Nginx.Types
import           NgxTop.UI
import           Pipes hiding (for)
import qualified Pipes.ByteString as PB
import qualified Pipes.Group as PG
import           System.IO.TailFile.Pipes
import           Types
import           URI.ByteString

run :: FilePath -> IO ()
run path = do
  db <- openGeoDBBS geoIPDB
  let initialStats =
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
  stats <- atomically $ newTVar initialStats
  eventChan <- newBChan 10
  a <- async $ void $ tailFile path (updateStats stats)
  b <-
    async $
    void $ do
      _ <-
        customMain
          (mkVty defaultConfig)
          (Just eventChan)
          (app path)
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
  (_, ret) <- waitAnyCatchCancel [a, b, c]
  case ret of
    Left e -> print e
    Right _ -> return ()

lined
  :: Monad m
  => Producer ByteString m x -> Producer ByteString m x
lined p = Fold.purely PG.folds Fold.mconcat (view PB.lines p)

parseLine
  :: Monad m
  => Pipes.Proxy () ByteString () AccessLogEntry m b
parseLine =
  forever $ do
    x <- await
    case parseGateway x of
      Right l -> yield l
      Left _ -> return ()

updateStats
  :: MonadIO m
  => TVar Stats -> Producer ByteString m r -> m (void, r)
updateStats stats p = do
  _ <-
    runEffect $
    (p & lined) >-> parseLine >->
    forever
      (do x <- await
          liftIO . atomically $
            modifyTVar' stats $ \current ->
              current & cacheHitCount .~
              (if aleCacheStatus x == HIT
                 then succ (current ^. cacheHitCount)
                 else current ^. cacheHitCount) &
              cacheMissCount .~
              (if aleCacheStatus x /= HIT
                 then succ (current ^. cacheMissCount)
                 else current ^. cacheMissCount) &
              responseCodes .~
              IntMap.insertWith
                (+)
                (aleResponseCode x)
                1
                (current ^. responseCodes) &
              domains .~
              HashMap.insertWith (+) (aleHost x) 1 (current ^. domains) &
              urls .~
              HashMap.insertWith
                (+)
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
                 else current ^. responseTime + aleResponseTime x) &
              totalBandwidth .~
              current ^.
              totalBandwidth +
              aleResponseSize x & ips .~
              HashMap.insertWith (+) (fromIPv4 . aleIP $ x) 1 (current ^. ips))
  return (undefined :: void, undefined :: r)
