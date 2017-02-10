{-# LANGUAGE OverloadedStrings #-}

module Log.Nginx.Combined
  ( combined
  , lbsParseCombined
  , parseCombined
  , toLogLine
  ) where

import           Protolude

import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import           Data.Time
import           Log.Nginx.Common
import           Log.Nginx.Pretty
import           Log.Nginx.Types

combined :: Parser AccessLogEntry
combined =
  commonHeader <*> (space *> (Just <$> quotedValue <?> "referer")) <*>
  (space *> quotedValue <?> "user agent") <*>
  pure MISS <*>
  pure "0" <*>
  pure 0

lbsParseCombined :: LazyBytes.ByteString -> Either Text AccessLogEntry
lbsParseCombined s =
  either (Left . toS) Right (AL.eitherResult $ AL.parse combined s)

parseCombined :: ByteString -> Either Text AccessLogEntry
parseCombined s =
  either (Left . toS) Right (parseOnly (combined <* endOfInput) s)


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
    , wrap "\"" (fromMaybe "-" (aleReferer entry))
    , wrap
        "\""
        (case aleUserAgent entry of
           "" -> "-"
           x -> x)
    ]
