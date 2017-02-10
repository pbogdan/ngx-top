{-# LANGUAGE OverloadedStrings #-}

module Log.Nginx.Common
  ( requestLine
  , cacheStatus
  , plainValue
  , quotedValue
  , bracketedValue
  , inQuotes
  , failEither
  , commonHeader
  ) where

import           Protolude hiding (takeWhile, try)

import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.IP
import           Data.Time
import           Log.Nginx.Types
import qualified URI.ByteString as URI

{-# INLINABLE isAscii #-}
isAscii :: Char -> Bool
isAscii = (< toEnum 127)

{-# INLINABLE isToken #-}
isToken :: Char -> Bool
isToken w = isAscii w && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

{-# INLINABLE httpVersion #-}
httpVersion :: Parser ByteString
httpVersion =
  "HTTP/" *> takeWhile (\c -> isDigit c || c == '.') <?> "HTTP Version"

{-# INLINABLE requestLine #-}
requestLine :: Parser Request
requestLine =
  Request <$> (takeWhile1 isToken <* char8 ' ' <?> "request method") <*>
  (failEither . URI.parseRelativeRef URI.laxURIParserOptions =<<
   (takeWhile1 (/= ' ') <* char8 ' ' <?> "request uri")) <*>
  httpVersion <?> "request version"

{-# INLINABLE cacheStatus #-}
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

{-# INLINABLE commonHeader #-}
commonHeader
  :: Parser (Maybe ByteString -> ByteString -> CacheStatus -> ByteString -> Double -> AccessLogEntry)
commonHeader =
  AccessLogEntry <$> (toIPv4 <$> (decimal `sepBy` char '.')) <*>
  (space *> plainValue <?> "iden") <*>
  (space *> plainValue <?> "user") <*>
  (space *>
   (parseTimeM True defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" . toS =<<
    bracketedValue)) <*>
  (space *> peekChar' >>= \c ->
     if c == '"'
       then pure ""
       else plainValue <?> "host") <*>
  ((space <|> pure ' ') *> inQuotes requestLine <?> "request line") <*>
  (space *> decimal <?> "response code") <*>
  (space *> decimal <?> "bytes")

{-# INLINABLE skipQuote #-}
skipQuote :: Parser ()
skipQuote = skipWhile (== '\"') <?> "skip quote"

{-# INLINABLE quote #-}
quote, lbrack, rbrack :: Parser Word8
quote = char8 '\"' <?> "quote"

{-# INLINABLE lbrack #-}
lbrack = char8 '[' <?> "lbrack"

{-# INLINABLE rbrack #-}
rbrack = char8 ']' <?> "rbrack"

{-# INLINABLE plainValue #-}
plainValue :: Parser ByteString
plainValue = takeTill (== ' ')

{-# INLINABLE quotedValue #-}
quotedValue :: Parser ByteString
quotedValue = do
  _ <- quote
  res <- takeTill (== '\"')
  _ <- quote
  return res

{-# INLINABLE inQuotes #-}
inQuotes :: Parser a -> Parser a
inQuotes f = skipQuote *> f <* skipQuote

{-# INLINABLE bracketedValue #-}
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
