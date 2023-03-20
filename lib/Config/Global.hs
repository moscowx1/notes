{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Global where

import qualified Config.Auth as A
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, constructorTagModifier)
import Data.Aeson (defaultOptions)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { _connectionString :: Text
  , _poolConnections :: Int
  , _port :: Int
  , _authConfig :: A.Config
  }
  deriving (Show, Eq, Generic)

deriveJSON defaultOptions 
  { fieldLabelModifier = drop 1
  , constructorTagModifier = drop 1 
  } ''Config
