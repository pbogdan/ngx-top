module Log.Nginx.Types
  ( CacheStatus(..)
  , Request(..)
  , AccessLogEntry(..)
  ) where

import Protolude

import Data.IP
import Data.Time
import URI.ByteString

data CacheStatus
  = MISS
  | BYPASS
  | EXPIRED
  | STALE
  | UPDATING
  | REVALIDATED
  | HIT
  | SCARCE
  | UNKNOWN
  deriving (Ord, Eq, Show)

data Request = Request
  { requestMethod :: !ByteString
  , requestUri :: URIRef Relative
  , requestVersion :: !ByteString
  } deriving (Eq, Ord, Show)

data AccessLogEntry = AccessLogEntry
  { aleIP :: !IPv4
  , aleIdent :: !ByteString
  , aleUser :: !ByteString
  , aleDate :: !UTCTime
  , aleHost :: !ByteString
  , aleRequest :: !Request
  , aleResponseCode :: !Int
  , aleResponseSize :: !Int
  , aleReferer :: Maybe ByteString
  , aleUserAgent :: !ByteString
  , aleCacheStatus :: CacheStatus
  , aleCacheConfig :: !ByteString
  , aleResponseTime :: !Double
  } deriving (Ord, Show, Eq)

