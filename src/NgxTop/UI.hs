{-# LANGUAGE ScopedTypeVariables #-}

module NgxTop.UI where

import           Protolude

import           Brick hiding (on)
import           Brick.Widgets.Border
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import           Graphics.Vty.Attributes (defAttr)
import           Types

pad :: Int -> a -> [a] -> [a]
pad n x xs
  | length xs >= n = xs
  | otherwise = xs ++ replicate (n - length xs) x

responseCodesWidget :: Stats -> Widget ()
responseCodesWidget stats =
  let codes = responseCodes stats
      lines =
        map
          (\(code, count) -> str (pad 15 ' ' (show code <> ": " <> show count)))
          (IntMap.toAscList codes)
  in padLeft (Pad 1) $
     padRight (Pad 1) (str "Response codes:" <=> str " " <=> vBox lines)

cacheHitWidget :: Stats -> Widget ()
cacheHitWidget stats =
  let total = cacheHitCount stats + cacheMissCount stats
      (hitRatio :: Double) =
        fromIntegral (cacheHitCount stats * 100) / fromIntegral total
      (missRatio :: Double) =
        fromIntegral (cacheMissCount stats * 100) / fromIntegral total
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     str "Requests:" <=> str " " <=> str ("Total requests: " <> show total) <=>
     str ("Cache hits: " <> show (cacheHitCount stats)) <=>
     str ("Cache misses: " <> show (cacheMissCount stats)) <=>
     str
       ("Cache hit ratio: " <> show (round hitRatio :: Integer) <> " / " <>
        show (round missRatio :: Integer))

topDomainsWidget :: Stats -> Widget ()
topDomainsWidget stats =
  let topDomains =
        take 5 $ sortBy (flip compare `on` snd) $ HashMap.toList (domains stats)
      topDomains' =
        map
          (\(domain, count) ->
             str (pad 30 ' ' (toS domain) <> ": " <> show count))
          topDomains
  in padLeft (Pad 1) $
     padRight (Pad 1) $ str "Top domains:" <=> str " " <=> vBox topDomains'

topUrlsWidget :: Stats -> Widget ()
topUrlsWidget stats =
  let topUrls =
        take 5 $ sortBy (flip compare `on` snd) $ HashMap.toList (urls stats)
      topUrls' =
        map
          (\(domain, count) ->
             str (pad 80 ' ' (toS domain) <> ": " <> show count))
          topUrls
  in padLeft (Pad 1) $
     padRight (Pad 1) $ str "Top urls:" <=> str " " <=> vBox topUrls'

ui :: FilePath -> Stats -> Widget ()
ui path stats =
  hBox [str path] <=> hBorder <=>
  (responseCodesWidget stats <+>
   vBorder <+> cacheHitWidget stats <+> vBorder <+> topDomainsWidget stats) <=>
  hBorder <=>
  topUrlsWidget stats


app :: FilePath -> App Stats Update ()
app path =
  App
  { appDraw = draw path
  , appHandleEvent = eventHandler
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr []
  , appChooseCursor = neverShowCursor
  }

draw :: FilePath -> Stats -> [Widget ()]
draw path stats = [ui path stats]

data Update = Update Stats

eventHandler :: Stats -> BrickEvent n Update -> EventM n (Next Stats)
eventHandler _ (AppEvent (Update x)) = continue x
eventHandler stats e = resizeOrQuit stats e
