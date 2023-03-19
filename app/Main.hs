{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Api (Api(..), UserApi (..))
import Config (Config(..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Database.Persist.Postgresql (withPostgresqlPool, runSqlPool, runMigration, SqlBackend)
import Data.Text.Encoding (encodeUtf8)
import DataAccess.Data (migrateAll)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Generic (genericServeT)
import Servant (Application)
import Utils (throwLeft)

main :: IO ()
main = do
  Config {..} <- throwLeft <$> eitherDecodeFileStrict "config.json"
  runNoLoggingT do
    withPostgresqlPool
      (encodeUtf8 _connectionString)
      _poolConnections
      \pool -> do
        runSqlPool (runMigration migrateAll) pool
        liftIO $ run _port (server (runNoLoggingT . (`runSqlPool` pool)))


server 
  :: (forall a. ReaderT SqlBackend (NoLoggingT IO) a -> IO a) 
  -> Application
server _ = genericServeT liftIO
  Api
    { _user = UserApi
        { _create = undefined
        , _getAll = undefined
        }
    }