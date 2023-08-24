{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Aeson (eitherDecodeFileStrict)
import Config.Config (Config)

{-
conInfo :: ConnectInfo
conInfo =
  ConnectInfo
    { connectHost = "localhost"
    , connectPort = 5432
    , connectUser = "postgres"
    , connectPassword = "postgres"
    , connectDatabase = "notes"
    }
-}

readConfig :: IO Config
readConfig = eitherDecodeFileStrict "config.json" >>= \case
      Left e -> error e
      Right x -> pure x

main :: IO ()
main = do
  config <- readConfig
  print config
