{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Config (Config(..), ConnectInfo(..))
import Data.Aeson (decode, encode)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck (testProperty, Arbitrary (arbitrary))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [configTests]

instance Arbitrary Config where
  arbitrary = Config <$> arbitrary

instance Arbitrary ConnectInfo where
  arbitrary = ConnectInfo 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

configTests :: TestTree
configTests = testGroup "Config.hs test"
  [ testProperty "parsing Config" $ \c -> 
      pure (c :: Config) == decode (encode c)
  ]

