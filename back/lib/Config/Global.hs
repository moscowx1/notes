{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Global where

import qualified Config.Auth as A
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (constructorTagModifier, deriveJSON, fieldLabelModifier)
import Data.Text (Text)
import GHC.Generics (Generic)

data Config = Config
  { _connectionString :: Text
  , _poolConnections :: Int
  , _port :: Int
  , _authConfig :: A.Config
  , _logFile :: FilePath
  }
  deriving (Show, Eq, Generic)

deriveJSON
  defaultOptions
    { fieldLabelModifier = drop 1
    , constructorTagModifier = drop 1
    }
  ''Config
