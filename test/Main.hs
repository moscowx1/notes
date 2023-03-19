{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State (StateT (runStateT), modify, get)
import Data.Functor.Identity (Identity (Identity))
import Data.List (find)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import DataAccess.Data (User (userLogin, User))
import qualified Logic.Auth as Auth
import Logic.Auth (Config(..), Handle(..), login, register)
import Test.Tasty (testGroup, defaultMain, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "" [authTest]

handle :: Auth.Handle ((StateT [User]) Identity)
handle = Handle
  { _config = Auth.Config
      { _generatingIterCount = 2000
      , _hashedPasswordLength = 60
      , _saltLength = 20
      }
  , _generateSalt = (const . pure . encodeUtf8) "azjkl;sdf"
  , _currentTime = pure $ UTCTime
      { utctDay = fromOrdinalDate 2022 1
      , utctDayTime = secondsToDiffTime 12
      }
  , _addToDb = \user -> do
      same <- sameLoginUser (userLogin user)
      case same of
        Nothing -> modify (user:) >> pure (Just user)
        Just _ -> pure Nothing
  , _getUser = \login' -> do
      sameLoginUser login'
  }
    where
      sameLoginUser login' = do
        users <- get
        let sameLogin = find (\User{..} -> userLogin == login') users
        pure sameLogin

authTest :: TestTree
authTest = testGroup "Auth logic tests"
  [ testCase "logging after registering test" $ do
      let user = "AsdaSdzxcv"
      let password = "asdfgsdf"
      let Identity (reg, db) = runStateT (register handle user password) []
      let Identity (log', _) = runStateT (login handle user password) db
      reg @?= log'
  ]
