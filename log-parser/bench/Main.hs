{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude

import           Log.Nginx
import           Criterion.Main

testInput :: ByteString
testInput = "11.22.33.44 - - [09/Feb/2017:12:17:04 +0000] example.org \"GET /some-long-path/some-long-path/?var=4.4.2 HTTP/1.0\" 200 408 \"Mozilla/5.0 (Linux; Android 5.0.1; SAMSUNG GT-I9515 Build/LRX22C) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/2.1 Chrome/34.0.1847.76 Mobile Safari/537.36\" HIT 0 0.002"

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Log.Nginx"
        [bench "parseAccessLogEntry" $ whnf parseAccessLogEntry testInput]
    ]
