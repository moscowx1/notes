module Main where

import qualified HandlerTests.Logic.Register as RL
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain handlerTests

handlerTests :: TestTree
handlerTests = testGroup "Handler Tests" [RL.registerTests]
