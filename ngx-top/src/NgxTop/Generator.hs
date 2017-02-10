{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module NgxTop.Generator
  ( generateEntries
  ) where

import           Protolude

import qualified Data.ByteString as Bytes
import           Data.IP
import           Data.String
import           Data.Time
import           Log.Nginx.Gateway
import           Log.Nginx.Types
import           Pipes
import qualified Pipes.ByteString as PB
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.Read
import           URI.ByteString

randomIPString :: Gen String
randomIPString = do
  a <- elements [0 .. 254] :: Gen Int
  b <- elements [0 .. 254] :: Gen Int
  c <- elements [0 .. 254] :: Gen Int
  d <- elements [1 .. 254] :: Gen Int
  return $ intercalate "." $ map show [a, b, c, d]

instance Arbitrary IPv4 where
  arbitrary = do
    ipString <- randomIPString
    return $ read ipString

instance Arbitrary CacheStatus where
  arbitrary =
    oneof
      [ pure MISS
      , pure BYPASS
      , pure EXPIRED
      , pure STALE
      , pure UPDATING
      , pure REVALIDATED
      , pure HIT
      , pure SCARCE
      , pure UNKNOWN
      ]
instance Arbitrary (URIRef Relative) where
  arbitrary =
    RelativeRef <$> pure Nothing <*>
    oneof
      (map (pure . ("/" <>)) ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]) <*>
    (pure . Query $ []) <*>
    pure Nothing

instance Arbitrary Request where
  arbitrary =
    Request <$>
    oneof
      [ pure "GET"
      , pure "HEAD"
      , pure "POST"
      , pure "PUT"
      , pure "DELETE"
      , pure "TRACE"
      , pure "OPTIONS"
      , pure "CONNECT"
      , pure "PATCH"
      ] <*>
    arbitrary <*>
    oneof [pure "1.0", pure "1.1"]

newtype SampleLogEntry =
  SampleLogEntry AccessLogEntry

instance Arbitrary SampleLogEntry where
  arbitrary =
    SampleLogEntry <$>
    (AccessLogEntry <$> arbitrary <*> pure "-" <*> pure "-" <*>
     (parseTimeOrError True defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" .
      formatTime defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" <$>
      (arbitrary :: Gen UTCTime)) <*>
     oneof [pure "example.com", pure "example.net", pure "example.org"] <*>
     arbitrary <*>
     arbitrary `suchThat` (\x -> x > 100 && x < 999) <*>
     arbitrary `suchThat` (\x -> x > 100 && x < 999) <*>
     pure Nothing <*>
     ((fmap toS <$> listOf . elements $ (['a' .. 'z'] ++ ['-'])) `suchThat`
      ((> 0) . Bytes.length)) <*>
     arbitrary <*>
     oneof [pure "0", pure "1"] <*>
     (getPositive <$> arbitrary))

randomEntry
  :: MonadIO m
  => m AccessLogEntry
randomEntry = do
  SampleLogEntry x <- liftIO . generate $ arbitrary
  return x

entryProducer
  :: MonadIO m
  => Int -> Producer ByteString m x
entryProducer n =
  forever $ do
    entries <- replicateM n randomEntry
    for_ entries (yield . (<>) "\n" . toLogLine)
    liftIO . threadDelay $ 1000000
    return ()

generateEntries :: MonadIO m => Int -> m ()
generateEntries n = runEffect (entryProducer n >-> PB.stdout)
