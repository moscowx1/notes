{-# LANGUAGE TemplateHaskell #-}

module Config.Auth (Config(..)) where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (constructorTagModifier, fieldLabelModifier, deriveJSON)

data Config = Config
  { _generatingIterCount :: Int
  , _hashedPasswordLength :: Int
  , _saltLength :: Int
  } deriving (Eq, Show)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 1
  , constructorTagModifier = drop 1
  } ''Config