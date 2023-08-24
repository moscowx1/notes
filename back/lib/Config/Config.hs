{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Config.Config where

import qualified Config.Auth as A
import qualified Config.Database as D
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (constructorTagModifier, deriveJSON, fieldLabelModifier)
import Data.Int (Int64)
import GHC.Generics (Generic)

data Config = Config
  { _database :: D.Config
  , _port :: Int
  , _auth :: A.Config
  , _logFile :: FilePath
  , _selectLimit :: Int64
  }
  deriving (Show, Eq, Generic)

deriveJSON
  defaultOptions
    { fieldLabelModifier = drop 1
    , constructorTagModifier = drop 1
    }
  ''Config
