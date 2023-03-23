{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Api (Api (..), Auth (..))
import Config.Global (Config (..))
import Control.Monad.Except (runExceptT)
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
import Handler.Auth (register, signIn)
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

eithToStatus :: IO (Either String a) -> IO String
eithToStatus v = do
  v' <- v
  pure $ case v' of
    Left err -> err
    Right _ -> "Success"

server ::
  Config ->
  (forall a. SqlBack a -> IO a) ->
  Application
server conf runer =
  genericServeT
    liftIO
    Api
      { _auth =
          Auth
            { _register = \req -> do
                let func = register (_authConfig conf) req
                let res = runer $ runExceptT func
                eithToStatus res
            , _signIn = \req -> do
                let func = signIn (_authConfig conf) req
                let res = runer $ runExceptT func
                eithToStatus res
            }
      }
