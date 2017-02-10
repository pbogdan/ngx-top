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
  ) where

import Protolude hiding (to)

import Control.Lens
import Data.GeoIP2
import Data.HashMap.Strict (HashMap)

data Stats = Stats
  { _cacheHitCount :: !Int
  , _cacheMissCount :: !Int
  , _responseCodes :: !(IntMap Int)
  , _domains :: HashMap ByteString Int
  , _urls :: HashMap ByteString Int
  , _responseTimes :: HashMap ByteString (Int, Double)
  , _responseTime :: !Double
  , _totalBandwidth :: !Int
  , _ips :: HashMap [Int] Int
  , _updateCount :: !Int
  , _geoDB :: GeoDB
  }

$(makeLenses ''Stats)

totalRequests :: Getter Stats Int
totalRequests = to (\stats -> stats ^. cacheHitCount + stats ^. cacheMissCount)

requestsPerSecond :: Getter Stats Double
requestsPerSecond =
  to
    (\stats ->
       fromIntegral (stats ^. totalRequests) /
       fromIntegral (stats ^. updateCount))
