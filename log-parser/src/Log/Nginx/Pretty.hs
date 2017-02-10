{-# LANGUAGE OverloadedStrings #-}

module Log.Nginx.Pretty
  ( wrap
  , brackets
  , displayRequest
  ) where

import           Protolude

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LazyBytes
import           Log.Nginx.Types
import qualified URI.ByteString as URI

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
