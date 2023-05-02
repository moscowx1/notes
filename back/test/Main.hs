module Main where

import qualified AuthTest as AT
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test" [AT.tests] -- [TA.tests]