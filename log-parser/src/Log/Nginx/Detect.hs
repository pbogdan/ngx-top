module Log.Nginx.Detect
  ( detect
  ) where

import Protolude

import Log.Nginx.Types

hush' :: Either e a -> Maybe a
hush' (Left _) = Nothing
hush' (Right x) = Just x

detect
  :: [ByteString -> Either Text AccessLogEntry]
  -> ByteString
  -> Maybe (ByteString -> Either Text AccessLogEntry)
detect parsers s =
  getFirst . foldr (<>) (First Nothing) $
  map ((First . hush') . (\parser -> parser s >> return parser)) parsers
