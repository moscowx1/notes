{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Test.Tasty (testGroup, defaultMain, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Logic.Auth as Auth
import Logic.Auth (Config(..), Handle(..), login, register)
import Data.Functor.Identity (Identity)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import DataAccess.Data (User (userLogin, User))
import Data.List (find)
import Types (Login)
import Debug.Trace (traceShow)

main :: IO ()
main = defaultMain $ testGroup "" [authTest]


config :: Auth.Config
config = Auth.Config
  { generatingIterCount = 2000
  , hashedPasswordLength = 60
  , saltLength = 20
  }

db :: [User]
db = []

add :: User -> [User]
add = (: db)

get :: Login -> Maybe User
get login' = find (\User{..} -> userLogin == login') db

handle :: Auth.Handle Identity
handle = Auth.Handle
  { _config = config
  , _generateSalt = (const . pure . encodeUtf8) "azjkl;sdf"
  , _currentTime = pure $ UTCTime
    { utctDay = fromOrdinalDate 2022 1
    , utctDayTime = secondsToDiffTime 12
    }
  , _addToDb = \user -> do
     _ <- pure $ add user
     pure $ Just user
  , _getUser = pure . get
  }

authTest :: TestTree
authTest = testGroup "Auth logic tests"
  [ testCase "register login test" $ do
      let user = "AsdaSdzxcv"
      let password = "asdfgsdf"
      let regRes = register handle user password
      let _ = traceShow db ()
      let logRes = login handle user password
      logRes @?= regRes
  ]