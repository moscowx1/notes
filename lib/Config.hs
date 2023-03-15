{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config (Config(..), ConnectInfo(..)) where

import Data.Aeson ((.=), (.:), toJSON)
import qualified Data.Aeson as A
import Data.Word (Word16)

data ConnectInfo = ConnectInfo
  { connectHost :: String
  , connectPort :: Word16
  , connectUser :: String
  , connectPassword :: String
  , connectDatabase :: String
  } deriving (Show, Eq)

newtype Config = Config { _dbConnect :: ConnectInfo }
  deriving (Show, Eq)

instance A.FromJSON Config where
  parseJSON = A.withObject "Config" $ \o -> do
    conInfo <- o .: "dbConnect"
    pure $ Config conInfo

instance A.ToJSON Config where
  toJSON Config {..}= A.object [ "dbConnect" .= _dbConnect ]

instance A.ToJSON ConnectInfo where
  toJSON ConnectInfo {..} = A.object
    [ "host" .= toJSON connectHost
    , "port" .= toJSON connectPort
    , "user" .= toJSON connectUser
    , "password" .= toJSON connectPassword
    , "database" .= toJSON connectDatabase
    ]

instance A.FromJSON ConnectInfo where
  parseJSON = A.withObject "ConnectInfo" $ \o -> do
    host <- o .: "host"
    database <- o .: "database"
    pwd <- o .: "password"
    port <- o .: "port"
    user <- o .: "user"
    pure $ ConnectInfo 
      { connectDatabase = database
      , connectHost = host
      , connectPassword = pwd
      , connectPort = port
      , connectUser = user
      }

