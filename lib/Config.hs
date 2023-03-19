{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (Config(..)) where

import Data.Aeson (defaultOptions)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, constructorTagModifier)
import qualified Logic.Auth as LA

data Config = Config
  { _connectionString :: Text
  , _poolConnections :: Int
  , _port :: Int
  , _authConfig :: LA.Config
  }
  deriving (Show, Eq, Generic)

deriveJSON defaultOptions 
  { fieldLabelModifier = drop 1
  , constructorTagModifier = drop 1 
  } ''Config
