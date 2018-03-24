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
import           Data.GeoIP2
import qualified Data.HashMap.Strict as HashMap
import           Data.IP
import qualified Data.IntMap as IntMap
import           Graphics.Vty hiding (pad)
import           Types

pad
  :: TextWidth [a]
  => Int -> a -> [a] -> [a]
pad n x xs
  | textWidth xs >= n = xs
  | otherwise = xs ++ replicate (n - textWidth xs) x

responseCodeColor
  :: (Ord a, Num a, IsString t)
  => a -> t
responseCodeColor n
  | n >= 200 && n < 300 = "ok"
  | n >= 400 && n < 500 = "warning"
  | n >= 500 && n < 600 = "error"
  | otherwise = "black"

heading :: Int -> [Char] -> Widget n
heading n title =
  hLimit n $
  vLimit 1 (withAttr "header" $ str (pad n ' ' (" " <> title)) <=> fill ' ') <=>
  str " "

responseCodesWidget :: Stats -> Widget ()
responseCodesWidget stats =
  let codes = stats ^. responseCodes
      total = stats ^. totalRequests
      lines =
        map
          (\(code, count) ->
             withAttr (responseCodeColor code) $
             str
               (pad 15 ' ' (" " <> show code <> ": " <> (show . getSum $ count)) <>
                (show
                   (round'
                      (fromIntegral (getSum (count * 100)) / fromIntegral total)
                      1) <>
                 "%")))
          (IntMap.toAscList codes)
  in heading 30 "Response codes" <=> vBox lines

cacheHitWidget :: Stats -> Widget ()
cacheHitWidget stats =
  let total = stats ^. totalRequests
      (hitRatio :: Double) =
        fromIntegral (getSum (stats ^. cacheHitCount) * 100) /
        fromIntegral total
      (missRatio :: Double) =
        fromIntegral (getSum (stats ^. cacheMissCount) * 100) /
        fromIntegral total
  in heading 30 "Requests" <=> str (" Total requests: " <> show total) <=>
     str (" Cache hits: " <> (show . getSum $ stats ^. cacheHitCount)) <=>
     str (" Cache misses: " <> (show . getSum $ stats ^. cacheMissCount)) <=>
     str
       (" Cache hit ratio: " <> show (round hitRatio :: Integer) <> " / " <>
        show (round missRatio :: Integer)) <=>
     str (" Avg. rq / s: " <> show (round' (stats ^. requestsPerSecond) 2)) <=>
     str
       (" Avg. rq time: " <>
        show
          (round'
             (getSum (stats ^. responseTime) /
              fromIntegral (getSum (stats ^. cacheMissCount)))
             2)) <=>
     str
       (" Total Bandwidth: " <>
        show
          (round
             (fromIntegral (getSum (stats ^. totalBandwidth)) / 1024 / 1024 :: Double) :: Integer) <>
        " MB") <=>
     str
       (" Bandwidth / s: " <>
        show
          (round
             (fromIntegral (getSum (stats ^. totalBandwidth)) / 1024 / 1024 /
              fromIntegral (stats ^. updateCount) :: Double) :: Integer) <>
        " MB")

topDomainsWidget :: Stats -> Widget ()
topDomainsWidget stats =
  let topDomains =
        take 10 $
        sortBy (flip compare `on` snd) $ HashMap.toList (stats ^. domains)
      topDomains' =
        map
          (\(domain, count) ->
             str
               (" " <> pad 30 ' ' (toS (decodeUtf8 domain)) <> ": " <>
                (show . getSum $ count)))
          topDomains
  in heading 2000 "Top domains" <=> vBox topDomains'

topUrlsWidget :: Stats -> Widget ()
topUrlsWidget stats =
  let topUrls =
        take 10 $
        sortBy (flip compare `on` snd) $ HashMap.toList (stats ^. urls)
      topUrls' =
        map
          (\(domain, count) ->
             str
               (" " <> pad 80 ' ' (toS (decodeUtf8 domain)) <> ": " <>
                (show . getSum $ count)))
          topUrls
  in heading 2000 "Top urls by number of requests" <=> vBox topUrls'

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
               (" " <> pad 80 ' ' (toS (decodeUtf8 url)) <> ": " <>
                show
                  (round' (time / fromIntegral count) (2 :: Integer) :: Double)))
          topUrls
  in heading 2000 "Top MISS urls by average response time" <=> vBox lines

topBotsWidget :: Stats -> Widget ()
topBotsWidget stats =
  let topBots =
        take 10 $
        sortBy (flip compare `on` snd) $ HashMap.toList (stats ^. bots)
      topBots' =
        map
          (\(ua, count) ->
             str
               (" " <> pad 80 ' ' (toS (decodeUtf8 ua)) <> ": " <>
                (show . getSum $ count)))
          topBots
  in heading 2000 "Top known bots by number of requests" <=> vBox topBots'


topIPWidget :: Stats -> Widget ()
topIPWidget stats =
  let topIPs =
        take 10 $ sortBy (flip compare `on` snd) $ HashMap.toList (stats ^. ips)
      topIPs' =
        map
          (\(ip, count) ->
             str
               (" " <> pad 16 ' ' (show . toIPv4 $ ip) <> "(" <>
                toS
                  (either (const "") (fromMaybe "") $
                   geoCountryISO <$>
                   findGeoData (stats ^. geoDB) "en" (IPv4 . toIPv4 $ ip)) <>
                ")" <>
                ": " <>
                (show . getSum $ count)))
          topIPs
  in heading 30 "Top IP addresses" <=> vBox topIPs'

round'
  ::  Double -> Integer -> Double
round' f n = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)


ui :: FilePath -> Stats -> Widget ()
ui path stats =
  hBox [padLeft (Pad 1) $ padRight (Pad 1) $str ("ngx-top @ " <> path)] <=>
  hBorder <=>
  (responseCodesWidget stats <+>
   cacheHitWidget stats <+> topIPWidget stats <+> topDomainsWidget stats) <=>
  str " " <=>
  topUrlsWidget stats <=>
  str " " <=>
  responseTimesWidget stats <=>
  str " " <=>
  topBotsWidget stats

app :: FilePath -> App Stats Update ()
app path =
  App
    { appDraw = draw path
    , appHandleEvent = eventHandler
    , appStartEvent = return
    , appAttrMap =
        const $
        attrMap
          defAttr
          [ (attrName "bold", Attr (SetTo bold) Default Default Default)
          , (attrName "ok", fg green)
          , (attrName "warning", fg yellow)
          , (attrName "error", fg red)
          , ( attrName "header"
            , Attr Default (SetTo brightWhite) (SetTo green) Default)
          ]
    , appChooseCursor = neverShowCursor
    }

draw :: FilePath -> Stats -> [Widget ()]
draw path stats = [ui path stats]

data Update = Update Stats

eventHandler :: Stats -> BrickEvent n Update -> EventM n (Next Stats)
eventHandler _prev (AppEvent (Update current)) = continue current
eventHandler stats e = resizeOrQuit stats e
