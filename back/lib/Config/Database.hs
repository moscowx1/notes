{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Database where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (constructorTagModifier, deriveJSON, fieldLabelModifier)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { _host :: Text
  , _port :: Int
  , _user :: Text
  , _password :: Text
  , _database :: Text
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
