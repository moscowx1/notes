{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Database where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (constructorTagModifier, deriveJSON, fieldLabelModifier)
import Data.Word (Word16)
import GHC.Generics (Generic)

data Config = Config
  { _host :: String
  , _port :: Word16
  , _user :: String
  , _password :: String
  , _database :: String
  , _maxConnections :: Int
  , _keepAliveUnusedConnection :: Double
  }
  deriving (Show, Eq, Generic)

deriveJSON
  defaultOptions
    { fieldLabelModifier = drop 1
    , constructorTagModifier = drop 1
    }
  ''Config
