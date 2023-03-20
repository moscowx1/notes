{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Api (Api(..), UserApi (..))
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict)
import Types (SqlBack)
import Database.Persist.Postgresql
  ( withPostgresqlPool
  , runSqlPool
  , runMigration
  )
import Data.Text.Encoding (encodeUtf8)
import DataAccess.Data (migrateAll)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Generic (genericServeT)
import Servant (Application)
import Utils (throwLeft)
import Config.Global (Config(..))
import Handler.Auth (register)

main :: IO ()
main = do
  c <- throwLeft <$> eitherDecodeFileStrict "config.json"
  getPool c \pool -> do
      runSqlPool (runMigration migrateAll) pool
      runServer c pool

  where
    getPool Config {..} = runNoLoggingT . withPostgresqlPool
      (encodeUtf8 _connectionString)
      _poolConnections

    runServer c pool = liftIO $
      run (_port c) (server c (runNoLoggingT . (`runSqlPool` pool)))

server
  :: Config
  -> (forall a. SqlBack a -> IO a)
  -> Application
server conf runer = genericServeT liftIO
  Api
    { _user = UserApi
        { _register = \k -> do
            let f = register (_authConfig conf)
            user <- runer $ f k
            pure $ case user of
              Nothing -> False
              Just _ -> True
        , _getAll = undefined
        }
    }