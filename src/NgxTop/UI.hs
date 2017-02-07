{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NgxTop.UI
  ( Update(..)
  , app
  ) where

import           Protolude hiding ((&))

import           Brick hiding (on)
import           Brick.Widgets.Border
import           Control.Lens
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
  let codes = stats ^. responseCodes
      total = stats ^. totalRequests
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
  let total = stats ^. totalRequests
      (hitRatio :: Double) =
        fromIntegral (stats ^. cacheHitCount * 100) / fromIntegral total
      (missRatio :: Double) =
        fromIntegral (stats ^. cacheMissCount * 100) / fromIntegral total
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     withAttr "bold" (str "Requests:") <=> str " " <=>
     str ("Total requests: " <> show total) <=>
     str ("Cache hits: " <> show (stats ^. cacheHitCount)) <=>
     str ("Cache misses: " <> show (stats ^. cacheMissCount)) <=>
     str
       ("Cache hit ratio: " <> show (round hitRatio :: Integer) <> " / " <>
        show (round missRatio :: Integer)) <=>
     str ("Avg. rq / s: " <> show (round' (stats ^. requestsPerSecond) 2)) <=>
     str
       ("Avg. rq time: " <>
        show
          (round'
             (stats ^. responseTime / fromIntegral (stats ^. cacheMissCount))
             2)) <=>
     str
       ("Total bandwidth: " <>
        show
          (round
             (fromIntegral (stats ^. totalBandwidth) / 1024 / 1024 :: Double) :: Integer))

topDomainsWidget :: Stats -> Widget ()
topDomainsWidget stats =
  let topDomains =
        take 10 $
        sortBy (flip compare `on` snd) $ HashMap.toList (stats ^. domains)
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
        take 10 $
        sortBy (flip compare `on` snd) $ HashMap.toList (stats ^. urls)
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
        HashMap.toList (stats ^. responseTimes)
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
        take 10 $ sortBy (flip compare `on` snd) $ HashMap.toList (stats ^. ips)
      topIPs' =
        map
          (\(ip, count) ->
             str (pad 16 ' ' (show . toIPv4 $ ip) <> ": " <> show count))
          topIPs
  in padLeft (Pad 1) $
     padRight (Pad 1) $
     withAttr "bold" (str "Top ips by number of requests:") <=> str " " <=>
     vBox topIPs'

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
eventHandler _prev (AppEvent (Update current)) = continue current
eventHandler stats e = resizeOrQuit stats e
