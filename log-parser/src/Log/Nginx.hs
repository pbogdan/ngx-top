{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Log.Nginx
  ( lbsParseAccessLogEntry
  , parseAccessLogEntry
  , accessLogEntry
  , Request(..)
  , AccessLogEntry(..)
  , CacheStatus(..)
  ) where

import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyBytes
import           Data.IP
import           Data.Time
import           Data.Word
import           Protolude hiding (takeWhile)
import           URI.ByteString (URIRef, Relative)
import qualified URI.ByteString as URI

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
  deriving (Eq, Show)

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
  , aleReq :: !Request
  , aleStatus :: !Int
  , aleBytes :: !Int
  , aleCacheStatus :: !ByteString
  , aleCacheConfig :: !ByteString
  , aleRespTime :: !Double
  } deriving (Ord, Show, Eq)

isAscii :: Char -> Bool
isAscii = (< toEnum 127)

isToken :: Char -> Bool
isToken w = isAscii w && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

httpVersion :: Parser ByteString
httpVersion =
  "HTTP/" *> takeWhile (\c -> isDigit c || c == '.') <?> "HTTP Version"

requestLine :: Parser Request
requestLine =
  Request <$> (takeWhile1 isToken <* char8 ' ' <?> "request method") <*>
  (failEither . URI.parseRelativeRef URI.laxURIParserOptions =<<
   (takeWhile1 (/= ' ') <* char8 ' ' <?> "request uri")) <*>
  httpVersion <?> "request version"

skipQuote :: Parser ()
skipQuote = skipWhile (== '\"') <?> "skip quote"

quote, lbrack, rbrack :: Parser Word8
quote = char8 '\"' <?> "quote"

{-# INLINE quote #-}
lbrack = char8 '[' <?> "lbrack"

{-# INLINE lbrack #-}
rbrack = char8 ']' <?> "rbrack"

{-# INLINE rbrack #-}
plainValue :: Parser ByteString
plainValue = takeTill (== ' ')

quotedValue :: Parser ByteString
quotedValue = do
  _ <- quote
  res <- takeTill (== '\"')
  _ <- quote
  return res

inQuotes :: Parser a -> Parser a
inQuotes f = skipQuote *> f <* skipQuote

bracketedValue :: Parser ByteString
bracketedValue = do
  _ <- lbrack
  res <- takeTill (== ']')
  _ <- rbrack
  return res

failEither
  :: (Show e, Monad m)
  => Either e a -> m a
failEither (Right x) = return x
failEither (Left e) = fail . show $ e

accessLogEntry :: Parser AccessLogEntry
accessLogEntry = do
  ip <- (failEither =<< readEither . toS <$> plainValue) <?> "ip"
  _ <- space
  iden <- plainValue
  _ <- space
  user <- plainValue
  _ <- space
  date <-
    parseTimeM True defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" . toS =<<
    bracketedValue
  _ <- space
  host <- plainValue
  _ <- space
  request <- inQuotes requestLine
  _ <- space
  status <- decimal <?> "status"
  _ <- space
  bytes <- decimal
  _ <- space
  _ <- quotedValue -- user agent
  _ <- space
  cacheStatus <- plainValue
  _ <- space
  cacheConfig <- plainValue
  _ <- space
  responseTime <- double <?> "responseTime"
  return $
    AccessLogEntry
      ip
      iden
      user
      date
      host
      request
      status
      bytes
      cacheStatus
      cacheConfig
      responseTime

rpcAccessLogEntry :: Parser AccessLogEntry
rpcAccessLogEntry = do
  ip <- (failEither =<< readEither . toS <$> plainValue) <?> "ip"
  _ <- space
  iden <- plainValue
  _ <- space
  user <- plainValue
  _ <- space
  date <-
    parseTimeM True defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" . toS =<<
    bracketedValue
  _ <- space
  host <- plainValue
  _ <- space
  request <- inQuotes requestLine
  _ <- space
  status <- decimal <?> "status"
  _ <- space
  bytes <- decimal
  _ <- space
  _ <- quotedValue -- user agent
  _ <- space
  cacheStatus <- plainValue <?> "cache status"
  _ <- space
  cacheConfig <- plainValue <?> "cache config"
  _ <- space
  _ <- string "rpc@"
  _rpcMethod <- plainValue <?> "rpc method"
  _ <- space
  _rpcLogin <- plainValue <?> "rpc login"
  _ <- space
  responseTime <- double <?> "responseTime"
  return $
    AccessLogEntry
      ip
      iden
      user
      date
      host
      request
      status
      bytes
      cacheStatus
      cacheConfig
      responseTime

lbsParseAccessLogEntry :: LazyBytes.ByteString -> Either Text AccessLogEntry
lbsParseAccessLogEntry s =
  either
    (Left . toS)
    Right
    (AL.eitherResult $ AL.parse (accessLogEntry <|> rpcAccessLogEntry) s)

parseAccessLogEntry :: ByteString -> Either Text AccessLogEntry
parseAccessLogEntry s =
  either
    (Left . toS)
    Right
    (parseOnly (accessLogEntry <|> rpcAccessLogEntry <* endOfInput) s)
