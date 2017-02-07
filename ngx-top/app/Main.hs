module Main where

import Protolude

import NgxTop

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putText "Usage: ngx-top logfile"
    [x] -> run x
    _ -> putText "Usage: ngx-top logfile"
