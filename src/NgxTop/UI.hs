{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NgxTop.UI
  ( Update(..)
  , app
  ) where

import           Protolude

import           Brick hiding (on)
import           Brick.Widgets.Border
import qualified Data.HashMap.Strict as HashMap
import           Data.IP
import qualified Data.IntMap as IntMap
import           Graphics.Vty.Attributes
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
      total = cacheHitCount stats + cacheMissCount stats
      lines =
        map
          (\(code, count) ->
             str
               (pad 15 ' ' (show code <> ": " <> show count) <>
                (show
                   (round' (fromIntegral (count * 100) / fromIntegral total) 1) <>
                 "%")))
          (IntMap.toAscList codes)
  in padLeft (Pad 1) $
     padRight
       (Pad 1)
       (withAttr "bold" (str "Response codes:") <=> str " " <=> vBox lines)

cacheHitWidget :: Stats -> Widget ()
cacheHitWidget stats =
  let total = cacheHitCount stats + cacheMissCount stats
      (hitRatio :: Double) =
        fromIntegral (cacheHitCount stats * 100) / fromIntegral total
      (missRatio :: Double) =
        fromIntegral (cacheMissCount stats * 100) / fromIntegral total
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     withAttr "bold" (str "Requests:") <=> str " " <=>
     str ("Total requests: " <> show total) <=>
     str ("Cache hits: " <> show (cacheHitCount stats)) <=>
     str ("Cache misses: " <> show (cacheMissCount stats)) <=>
     str
       ("Cache hit ratio: " <> show (round hitRatio :: Integer) <> " / " <>
        show (round missRatio :: Integer)) <=>
     str ("Avg. rq / s: " <> show (round' (requestsPerSecond stats) 2)) <=>
     str
       ("Avg. rq time: " <>
        show
          (round'
             (responseTime stats /
              fromIntegral (cacheMissCount stats))
             2)) <=>
     str
       ("Total bandwidth: " <>
        show
          (round (fromIntegral (totalBandwidth stats) / 1024 / 1024 :: Double) :: Integer))

topDomainsWidget :: Stats -> Widget ()
topDomainsWidget stats =
  let topDomains =
        take 10 $
        sortBy (flip compare `on` snd) $ HashMap.toList (domains stats)
      topDomains' =
        map
          (\(domain, count) ->
             str (pad 30 ' ' (toS (decodeUtf8 domain)) <> ": " <> show count))
          topDomains
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     withAttr "bold" (str "Top domains:") <=> str " " <=> vBox topDomains'

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
     withAttr "bold" (str "Top urls by number of requests:") <=> str " " <=>
     vBox topUrls'

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
     withAttr "bold" (str "Top MISS urls by average response time:") <=> str " " <=>
     vBox lines

topIPWidget :: Stats -> Widget ()
topIPWidget stats =
  let topIPs =
        take 10 $ sortBy (flip compare `on` snd) $ HashMap.toList (ips stats)
      topIPs' =
        map
          (\(ip, count) ->
             str (pad 16 ' ' (show . toIPv4 $ ip) <> ": " <> show count))
          topIPs
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     withAttr "bold" (str "Top ips by number of requests:") <=> str " " <=> vBox topIPs'

round'
  ::  Double -> Integer -> Double
round' f n = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)


ui :: FilePath -> Stats -> Widget ()
ui path stats =
  hBox [padLeft (Pad 1) $ padRight (Pad 1) $str ("ngx-top @ " <> path)] <=>
  hBorder <=>
  (responseCodesWidget stats <+>
   vBorder <+>
   cacheHitWidget stats <+>
   vBorder <+> topDomainsWidget stats <+> vBorder <+> topIPWidget stats) <=>
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
  , appAttrMap =
      const $
      attrMap defAttr [(attrName "bold", Attr (SetTo bold) Default Default)]
  , appChooseCursor = neverShowCursor
  }

draw :: FilePath -> Stats -> [Widget ()]
draw path stats = [ui path stats]

data Update = Update Stats

eventHandler :: Stats -> BrickEvent n Update -> EventM n (Next Stats)
eventHandler _prev (AppEvent (Update current)) =
  continue
    (current
     { requestsPerSecond =
         fromIntegral (cacheHitCount current + cacheMissCount current) /
         fromIntegral (updateCount current)
     })
eventHandler stats e = resizeOrQuit stats e
