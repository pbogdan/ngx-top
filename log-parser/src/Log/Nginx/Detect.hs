module Log.Nginx.Detect
  ( detect
  ) where

import Protolude

import Log.Nginx.Combined
import Log.Nginx.Gateway
import Log.Nginx.Types


hush' :: Either e a -> Maybe a
hush' (Left _) = Nothing
hush' (Right x) = Just x

detect :: ByteString -> Maybe (ByteString -> Either Text AccessLogEntry)
detect s =
  getFirst . foldr (<>) (First Nothing) $
  map
    (First . hush')
    [ parseGateway s >> return parseGateway
    , parseCombined s >> return parseCombined
    ]
