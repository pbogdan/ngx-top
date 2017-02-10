{-# LANGUAGE TemplateHaskell#-}
module GeoIP where

import Protolude

import Data.FileEmbed

geoIPDB :: ByteString
geoIPDB = $(embedFile "GeoLite2-Country.mmdb")
