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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib (main) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Config (Config(..))
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import Database.Persist.TH (mkPersist, sqlSettings, mkMigrate, persistLowerCase, share)
import Database.Persist.Postgresql (runMigration, withPostgresqlPool, runSqlPool, SqlBackend)
import Control.Monad.Cont (MonadIO(liftIO))
import Data.Aeson (eitherDecodeFileStrict, FromJSON, ToJSON)
import Utils (throwLeft)
import Servant (Application, Get)
import Servant.API (Post, ReqBody, NamedRoutes, (:>), (:-), JSON)
import Servant.Server.Generic (genericServeT)
import Control.Monad.Reader (ReaderT)
import Network.Wai.Handler.Warp (run)
import Servant.API.Generic (Generic)

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

data CreateUserReq = CreateUserReq
  { login :: Text
  , password :: Text
  } deriving (Show, Eq, Generic, FromJSON)

data UserDto = UserDto
  { login :: Text
  , createdAt :: UTCTime
  } deriving (Show, Eq, Generic, ToJSON)

data UserApi routes = UserApi
  { _create :: routes :- "create" :> ReqBody '[JSON] CreateUserReq :> Post '[JSON] (Maybe UserDto)
  , _getAll :: routes :- "get-all" :> Get '[JSON] [UserDto]
  } deriving (Generic)

data Api routes = Api
  { _user :: routes :- "user" :> NamedRoutes UserApi
  } deriving (Generic)

main :: IO ()
main = do
  Config {..} <- throwLeft <$> eitherDecodeFileStrict "config.json"
  runNoLoggingT do
    withPostgresqlPool (encodeUtf8 _connectionString) _poolConnections \pool -> do
      runSqlPool (runMigration migrateAll) pool
      liftIO $ run _port (server (`runSqlPool` pool))

server :: (forall a. ReaderT SqlBackend (NoLoggingT IO) a -> NoLoggingT IO a) -> Application
server _ = genericServeT liftIO
  Api
    { _user = UserApi
      { _create = undefined
      , _getAll = do
          cur <- getCurrentTime
          pure [UserDto "test" cur]
      }
    }
