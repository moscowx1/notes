{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (main) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Config (Config(..))
import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.TH (mkPersist, sqlSettings, mkMigrate, persistLowerCase, share)
import Database.Persist.Postgresql (runMigration, withPostgresqlPool, runSqlPool)
import Control.Monad.Cont (MonadIO(liftIO))
import Data.Aeson (eitherDecodeFileStrict)
import Utils (throwLeft)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  login Text
  salt Text
  password Text
  createdAt UTCTime default=now()
  deriving Show
Note
  content Text
  author UserId
  deriving Show
|]

main :: IO ()
main = do
  Config {..} <- throwLeft <$> eitherDecodeFileStrict "config.json"
  runNoLoggingT $ 
    withPostgresqlPool (encodeUtf8 _connectionString) _poolConnections \pool -> do
      runSqlPool (runMigration migrateAll) pool
      liftIO $ putStrLn "hi"

