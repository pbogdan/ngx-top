{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Log.Nginx
  ( lbsParseAccessLogEntry
  , parseAccessLogEntry
  , accessLogEntry
  , toLogLine
  , Request(..)
  , AccessLogEntry(..)
  , CacheStatus(..)
  ) where

import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Builder as BB
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
  , aleUserAgent :: !ByteString
  , aleCacheStatus :: CacheStatus
  , aleCacheConfig :: !ByteString
  , aleResponseTime :: !Double
  } deriving (Ord, Show, Eq)

wrap :: Semigroup a => a -> a -> a
wrap x y = x <> y <> x

brackets
  :: (IsString a, Semigroup a)
  => a -> a
brackets x = "[" <> x <> "]"

displayRequest :: AccessLogEntry -> ByteString
displayRequest entry =
  (requestMethod . aleRequest $ entry) <>
  wrap
    " "
    (LazyBytes.toStrict .
     BB.toLazyByteString . URI.serializeURIRef . requestUri . aleRequest $
     entry) <>
  "HTTP/" <>
  (requestVersion . aleRequest $ entry)

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

cacheStatus :: Parser CacheStatus
cacheStatus = do
  status <- plainValue <?> "cache status"
  case status of
    "MISS" -> pure MISS
    "BYPASS" -> pure BYPASS
    "EXPIRED" -> pure EXPIRED
    "STALE" -> pure STALE
    "UPDATING" -> pure UPDATING
    "REVALIDATED" -> pure REVALIDATED
    "HIT" -> pure HIT
    "SCARCE" -> pure SCARCE
    _ -> pure UNKNOWN

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

header :: Parser (Double -> AccessLogEntry)
header =
  AccessLogEntry <$> ((failEither =<< readEither . toS <$> plainValue) <?> "ip") <*>
  (space *> plainValue <?> "iden") <*>
  (space *> plainValue <?> "user") <*>
  (space *>
   (parseTimeM True defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" . toS =<<
    bracketedValue)) <*>
  (space *> plainValue <?> "host") <*>
  (space *> inQuotes requestLine <?> "request line") <*>
  (space *> decimal <?> "response code") <*>
  (space *> decimal <?> "bytes") <*>
  (space *> quotedValue <?> "user agent") <*>
  (space *> cacheStatus <?> "cache status") <*>
  (space *> plainValue <?> "cache config")

accessLogEntry :: Parser AccessLogEntry
accessLogEntry = header <*> (space *> double) <?> "response time"

rpcAccessLogEntry :: Parser AccessLogEntry
rpcAccessLogEntry =
  header <*>
  ((space *> string "rpc@" *> (plainValue <?> "rpc method") *> space *>
    plainValue <?> "rpc login") *>
   space *>
   double <?> "response time")

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
