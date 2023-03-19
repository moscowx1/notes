{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Api (Api(..), UserApi (..))
import Config (Config(..))
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Database.Persist.Postgresql 
  ( withPostgresqlPool
  , runSqlPool
  , runMigration
  )
import Data.Text.Encoding (encodeUtf8)
import DataAccess.Data (migrateAll)
import Network.Wai.Handler.Warp (run)
import Types(SqlRuner)
import Servant.Server.Generic (genericServeT)
import Servant (Application)
import Utils (throwLeft)

main :: IO ()
main = do
  c <- throwLeft <$> eitherDecodeFileStrict "config.json"
  getPool c \pool -> do
      runSqlPool (runMigration migrateAll) pool
      runServer c pool

  where
    getPool Config{..} = runNoLoggingT . withPostgresqlPool
      (encodeUtf8 _connectionString)
      _poolConnections

    runServer Config {..} pool = liftIO $
      run _port (server (runNoLoggingT . (`runSqlPool` pool)))

server 
  :: (forall a. SqlRuner a)
  -> Application
server _ = genericServeT liftIO
  Api
    { _user = UserApi
        { _create = undefined
        , _getAll = undefined
        }
    }