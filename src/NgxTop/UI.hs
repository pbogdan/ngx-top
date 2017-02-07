{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NgxTop.UI where

import           Protolude

import           Brick hiding (on)
import           Brick.Widgets.Border
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import           Graphics.Vty.Attributes (defAttr)
import           Types

pad
  :: TextWidth [a]
  => Int -> a -> [a] -> [a]
pad n x xs
  | textWidth xs >= n = xs
  | otherwise = xs ++ replicate (n - textWidth xs) x

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
        show (round missRatio :: Integer)) <=>
     str ("Requests / s: " <> show (requestsPerSecond stats)) <=>
     str
       ("Total bandwidth: " <>
        show
          (round (fromIntegral (totalBandwidth stats) / 1024 / 1024 :: Double)))

topDomainsWidget :: Stats -> Widget ()
topDomainsWidget stats =
  let topDomains =
        take 10 $ sortBy (flip compare `on` snd) $ HashMap.toList (domains stats)
      topDomains' =
        map
          (\(domain, count) ->
             str (pad 30 ' ' (toS (decodeUtf8 domain)) <> ": " <> show count))
          topDomains
  in padLeft (Pad 1) $
     padRight (Pad 1) $ str "Top domains:" <=> str " " <=> vBox topDomains'

topUrlsWidget :: Stats -> Widget ()
topUrlsWidget stats =
  let topUrls =
        take 10 $ sortBy (flip compare `on` snd) $ HashMap.toList (urls stats)
      topUrls' =
        map
          (\(domain, count) ->
             str (pad 80 ' ' (toS (decodeUtf8 domain)) <> ": " <> show count))
          topUrls
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     str "Top urls by number of requests:" <=> str " " <=> vBox topUrls'

responseTimesWidget :: Stats -> Widget ()
responseTimesWidget stats =
  let topUrls =
        take 10 $
        sortBy
          (\(_, (a, b)) (_, (c, d)) ->
             compare (d / fromIntegral c) (b / fromIntegral a)) $
        HashMap.toList (responseTimes stats)
      lines =
        map
          (\(url, (count, time)) ->
             str
               (pad 80 ' ' (toS (decodeUtf8 url)) <> ": " <>
                show
                  (round' (time / fromIntegral count) (2 :: Integer) :: Double)))
          topUrls
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     str "Top MISS urls by average response time:" <=> str " " <=> vBox lines

round'
  :: (Fractional c, Integral b, RealFrac a)
  => a -> b -> c
round' f n = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)

ui :: FilePath -> Stats -> Widget ()
ui path stats =
  hBox [str ("ngx-top @ " <> path)] <=> hBorder <=>
  (responseCodesWidget stats <+>
   vBorder <+> cacheHitWidget stats <+> vBorder <+> topDomainsWidget stats) <=>
  hBorder <=>
  topUrlsWidget stats <=>
  hBorder <=>
  responseTimesWidget stats


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
eventHandler prev (AppEvent (Update current)) =
  continue
    (current
     { requestsPerSecond =
         (cacheHitCount current + cacheMissCount current) -
         (cacheHitCount prev + cacheMissCount prev)
     })
eventHandler stats e = resizeOrQuit stats e
