module Types where

import Protolude

import Data.HashMap.Strict (HashMap)

data Stats = Stats
  { cacheHitCount :: !Int
  , cacheMissCount :: !Int
  , responseCodes :: !(IntMap Int)
  , domains :: HashMap ByteString Int
  , urls :: HashMap ByteString Int
  , responseTimes :: HashMap ByteString (Int, Double)
  , requestsPerSecond :: !Double
  , totalBandwidth :: !Int
  , ips :: HashMap [Int] Int
  , updateCount :: !Int
  } deriving (Eq, Show)
