{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Log.Nginx.Gateway
  ( lbsParseGateway
  , parseGateway
  , gateway
  , toLogLine
  ) where

import           Protolude hiding (takeWhile)

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import           Data.Time
import           Log.Nginx.Common
import           Log.Nginx.Pretty
import           Log.Nginx.Types

header :: Parser (Double -> AccessLogEntry)
header =
  commonHeader <*> pure Nothing <*> (space *> quotedValue <?> "user agent") <*>
  (space *> cacheStatus <?> "cache status") <*>
  (space *> plainValue <?> "cache config")

gateway :: Parser AccessLogEntry
gateway = header <*> (space *> double) <?> "response time"

gatewayRPC :: Parser AccessLogEntry
gatewayRPC =
  header <*>
  ((space *> string "rpc@" *> (plainValue <?> "rpc method") *> space *>
    plainValue <?> "rpc login") *>
   space *>
   double <?> "response time")

lbsParseGateway :: LazyBytes.ByteString -> Either Text AccessLogEntry
lbsParseGateway s =
  either
    (Left . toS)
    Right
    (AL.eitherResult $ AL.parse (gateway <|> gatewayRPC) s)

parseGateway :: ByteString -> Either Text AccessLogEntry
parseGateway s =
  either (Left . toS) Right (parseOnly (gateway <|> gatewayRPC <* endOfInput) s)

toLogLine :: AccessLogEntry -> ByteString
toLogLine entry =
  Bytes.intercalate
    " "
    [ show . aleIP $ entry
    , case aleIdent entry of
        "" -> "-"
        x -> x
    , case aleUser entry of
        "" -> "-"
        x -> x
    , brackets . toS . formatTime defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" $
      aleDate entry
    , aleHost entry
    , wrap "\"" (displayRequest entry)
    , show . aleResponseCode $ entry
    , show . aleResponseSize $ entry
    , wrap
        "\""
        (case aleUserAgent entry of
           "" -> "-"
           x -> x)
    , case aleCacheStatus entry of
        UNKNOWN -> "-"
        x -> show x
    , aleCacheConfig entry
    , show . aleResponseTime $ entry
    ]
