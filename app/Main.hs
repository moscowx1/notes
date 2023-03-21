{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Api (Api (..), UserApi (..))
import Config.Global (Config (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Text.Encoding (encodeUtf8)
import DataAccess.Data (migrateAll)
import Database.Persist.Postgresql (
  runMigration,
  runSqlPool,
  withPostgresqlPool,
 )
import Handler.Auth (register)
import Network.Wai.Handler.Warp (run)
import Servant (Application)
import Servant.Server.Generic (genericServeT)
import Types (SqlBack)
import Utils (throwLeft)

main :: IO ()
main = do
  c <- throwLeft <$> eitherDecodeFileStrict "config.json"
  getPool c \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runServer c pool
 where
  getPool Config{..} =
    runNoLoggingT
      . withPostgresqlPool
        (encodeUtf8 _connectionString)
        _poolConnections

  runServer c pool =
    liftIO $
      run (_port c) (server c (runNoLoggingT . (`runSqlPool` pool)))

server ::
  Config ->
  (forall a. SqlBack a -> IO a) ->
  Application
server conf runer =
  genericServeT
    liftIO
    Api
      { _user =
          UserApi
            { _register = \k -> do
                let f = register (_authConfig conf)
                user <- runer $ f k
                pure $ case user of
                  Nothing -> False
                  Just _ -> True
            , _getAll = undefined
            }
      }
