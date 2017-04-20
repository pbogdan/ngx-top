{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  ( Stats(..)
  , cacheHitCount
  , cacheMissCount
  , domains
  , ips
  , responseCodes
  , responseTime
  , responseTimes
  , totalBandwidth
  , updateCount
  , urls
  , totalRequests
  , requestsPerSecond
  , geoDB
  , botsCache
  , bots
  ) where

import Protolude hiding (to)

import Control.Lens
import Data.GeoIP2
import Data.HashMap.Strict (HashMap)

data Stats = Stats
  { _cacheHitCount :: !(Sum Int)
  , _cacheMissCount :: !(Sum Int)
  , _responseCodes :: !(IntMap (Sum Int))
  , _domains :: HashMap ByteString (Sum Int)
  , _urls :: HashMap ByteString (Sum Int)
  , _responseTimes :: HashMap ByteString (Int, Double)
  , _responseTime :: !(Sum Double)
  , _totalBandwidth :: !(Sum Int)
  , _ips :: HashMap [Int] (Sum Int)
  , _updateCount :: !Int
  , _geoDB :: GeoDB
  , _botsCache :: HashMap ByteString (Maybe ByteString)
  , _bots :: HashMap ByteString (Sum Int)
  }

$(makeLenses ''Stats)

totalRequests :: Getter Stats Int
totalRequests =
  to (\stats -> getSum (stats ^. cacheHitCount <> stats ^. cacheMissCount))

requestsPerSecond :: Getter Stats Double
requestsPerSecond =
  to
    (\stats ->
       fromIntegral (stats ^. totalRequests) /
       fromIntegral (stats ^. updateCount))
