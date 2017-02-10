{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude

import Criterion.Main
import Log.Nginx.Combined
import Log.Nginx.Gateway

gatewayInput :: ByteString
gatewayInput = "11.22.33.44 - - [09/Feb/2017:12:17:04 +0000] example.org \"GET /some-long-path/some-long-path/?var=4.4.2 HTTP/1.0\" 200 408 \"Mozilla/5.0 (Linux; Android 5.0.1; SAMSUNG GT-I9515 Build/LRX22C) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/2.1 Chrome/34.0.1847.76 Mobile Safari/537.36\" HIT 0 0.002"

combinedInput :: ByteString
combinedInput =
  "127.0.0.1 - - [07/Feb/2017:13:33:55 +0000] \"GET /wp-content/plugins/woocommerce/assets/css/woocommerce-layout.css?ver=2.6.13 HTTP/1.1\" 200 14668 \"http://testwp/\" \"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.76 Safari/537.36\""

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Log.Nginx"
        [ bench "parseGateway" $ whnf parseGateway gatewayInput
        , bench "parseCombined" $ whnf parseCombined combinedInput
        ]
    ]
