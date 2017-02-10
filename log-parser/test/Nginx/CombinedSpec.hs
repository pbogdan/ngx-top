{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nginx.CombinedSpec
  ( main
  , spec
  )

where

import           Protolude

import qualified Data.ByteString as Bytes
import           Data.IP
import           Data.String
import           Data.Time
import           Log.Nginx.Combined
import           Log.Nginx.Types
import           Test.Hspec
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
  arbitrary = pure MISS

instance Arbitrary (URIRef Relative) where
  arbitrary =
    RelativeRef <$> pure Nothing <*>
    (toS . ("/" <>) . intercalate "/" <$>
     listOf ((listOf . elements $ ['a' .. 'z']) `suchThat` (not . null))) <*>
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

instance Arbitrary AccessLogEntry where
  arbitrary =
    AccessLogEntry <$> arbitrary <*>
    ((fmap toS <$> listOf . elements $ (['a' .. 'z'] ++ ['-'])) `suchThat`
     ((> 0) . Bytes.length)) <*>
    ((fmap toS <$> listOf . elements $ (['a' .. 'z'] ++ ['-'])) `suchThat`
     ((> 0) . Bytes.length)) <*>
    (parseTimeOrError True defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" .
     formatTime defaultTimeLocale "%d/%h/%Y:%H:%M:%S %z" <$>
     (arbitrary :: Gen UTCTime)) <*>
    (fmap toS <$> listOf . elements $ (['a' .. 'z'] ++ ['.'])) <*>
    arbitrary <*>
    arbitrary `suchThat` (\x -> x > 100 && x < 999) <*>
    arbitrary `suchThat` (\x -> x > 100 && x < 999) <*>
    (fmap (Just . toS) <$> listOf . elements $ (['a' .. 'z'] ++ ['.'])) <*>
    ((fmap toS <$> listOf . elements $ (['a' .. 'z'] ++ ['-'])) `suchThat`
     ((> 0) . Bytes.length)) <*>
    arbitrary <*>
    oneof [pure "0"] <*>
    pure 0

main :: IO ()
main = hspec spec

{-# ANN spec ("HLint: ignore Redundant do" :: Text) #-}
spec :: Spec
spec =
  describe "For trivial valid input" $ do
    it "parseCombined . toLogLine == id" $
      property $ \(entry :: AccessLogEntry) ->
        Right entry `shouldBe` parseCombined (toLogLine entry)
