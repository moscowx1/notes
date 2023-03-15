{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}


module Config (Config(..)) where

import Data.Aeson (defaultOptions)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, constructorTagModifier)

data Config = Config 
  { _connectionString :: Text
  , _poolConnections :: Int
  }
  deriving (Show, Eq, Generic)

deriveJSON defaultOptions 
  { fieldLabelModifier = drop 1
  , constructorTagModifier = drop 1 
  } ''Config

