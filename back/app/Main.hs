{-# LANGUAGE LambdaCase #-}

module Main (main) where

import qualified Config.Config as GC
import qualified Config.Database as DC
import Data.Aeson (eitherDecodeFileStrict)
import Data.Pool (PoolConfig, defaultPoolConfig, newPool)
import Database.Beam.Postgres (ConnectInfo (..), Connection, close, connect)

readConfig :: IO GC.Config
readConfig =
  eitherDecodeFileStrict "config.json" >>= \case
    Left e -> error e
    Right x -> pure x

createPool ::
  GC.Config ->
  PoolConfig Connection
createPool config =
  defaultPoolConfig
    (connect connectInfo)
    close
    (DC._keepAliveUnusedConnection dbConfig)
    (DC._maxConnections dbConfig)
 where
  dbConfig = GC._database config
  connectInfo =
    ConnectInfo
      { connectHost = DC._host dbConfig
      , connectPort = DC._port dbConfig
      , connectUser = DC._user dbConfig
      , connectPassword = DC._password dbConfig
      , connectDatabase = DC._database dbConfig
      }

main :: IO ()
main = do
  config <- readConfig
  _ <- newPool $ createPool config
  print "end"
