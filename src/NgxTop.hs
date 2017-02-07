{-# LANGUAGE ScopedTypeVariables #-}

module NgxTop where

import           Protolude hiding ((&))

import           Brick.BChan
import           Brick.Main
import           Control.Concurrent.STM.TVar
import           Control.Lens hiding (lined)
import qualified Data.HashMap.Strict as HashMap
import           Data.IP
import qualified Data.IntMap.Strict as IntMap
import           Graphics.Vty
import           Log.Nginx
import           NgxTop.UI
import           Pipes hiding (for)
import qualified Pipes.ByteString as PB
import qualified Pipes.Group as PG
import           System.IO.TailFile.Pipes
import           Types
import           URI.ByteString

run :: FilePath -> IO ()
run path = do
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
  stats <- atomically $ newTVar initialStats
  eventChan <- newBChan 10
  a <- async $ void $ tailFile path (updateStats stats)
  b <-
    async $
    void $
    forever $ do
      current <-
        atomically $ do
          modifyTVar' stats $ \current ->
            current {updateCount = updateCount current + 1}
          readTVar stats
      writeBChan eventChan $ Update current
      threadDelay 1000000
  c <-
    async $
    void $ do
      _ <-
        customMain
          (mkVty defaultConfig)
          (Just eventChan)
          (app path)
          initialStats
      return ()
  (_, ret) <- waitAnyCatchCancel [a, b, c]
  case ret of
    Left e -> print e
    Right _ -> return ()

lined
  :: Monad m
  => Producer ByteString m x -> Producer ByteString m x
lined = PG.concats . view PB.lines

parseLine
  :: Monad m
  => Pipes.Proxy () ByteString () AccessLogEntry m b
parseLine =
  forever $ do
    x <- await
    case parseAccessLogEntry x of
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
              current
              { cacheHitCount =
                  if aleCacheStatus x == "HIT"
                    then cacheHitCount current + 1
                    else cacheHitCount current
              , cacheMissCount =
                  if aleCacheStatus x /= "HIT"
                    then cacheMissCount current + 1
                    else cacheMissCount current
              , responseCodes =
                  IntMap.insertWith (+) (aleStatus x) 1 (responseCodes current)
              , domains = HashMap.insertWith (+) (aleHost x) 1 (domains current)
              , urls =
                  HashMap.insertWith
                    (+)
                    (aleHost x <> rrPath (requestUri $ aleReq x))
                    1
                    (urls current)
              , responseTimes =
                  if aleCacheStatus x == "HIT"
                    then responseTimes current
                    else HashMap.insertWith
                           (\(a, b) (c, d) -> (a + c, b + d))
                           (aleHost x <> rrPath (requestUri $ aleReq x))
                           (1, aleRespTime x)
                           (responseTimes current)
              , totalBandwidth = totalBandwidth current + aleBytes x
              , ips =
                  HashMap.insertWith (+) (fromIPv4 . aleIP $ x) 1 (ips current)
              })
  return (undefined :: void, undefined :: r)
