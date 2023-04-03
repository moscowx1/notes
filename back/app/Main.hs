{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Api (Api (..), Auth (..))
import Config.Global (Config (..))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Cors (corsMiddleware)
import Crypto.JOSE (JWK)
import Data.Aeson (decodeFileStrict, eitherDecodeFileStrict)
import Data.Text.Encoding (encodeUtf8)
import DataAccess.Data (migrateAll)
import Database.Persist.Postgresql (
  runMigration,
  runSqlPool,
  withPostgresqlPool,
 )
import Handler.Auth (register, signIn)
import JwtSupport ()
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (
  Application,
  Context (EmptyContext, (:.)),
 )
import Servant.Auth.Server (
  acceptLogin,
  defaultCookieSettings,
  defaultJWTSettings,
 )
import Servant.Server.Generic (genericServeTWithContext)
import Types (SqlBack)
import Utils (throwLeft)

main :: IO ()
main = do
  c <- throwLeft <$> eitherDecodeFileStrict "config.json"
  jwk <-
    decodeFileStrict "jwk.json" >>= \case
      Nothing -> error "error reading jwk"
      Just x -> pure x
  getPool c \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runServer jwk c pool
 where
  getPool Config{..} =
    runNoLoggingT
      . withPostgresqlPool
        (encodeUtf8 _connectionString)
        _poolConnections

  runServer jwk c pool =
    liftIO $
      run (_port c) (corsMiddleware . logStdoutDev $ server jwk c (runNoLoggingT . (`runSqlPool` pool)))

eithToStatus :: IO (Either String a) -> IO String
eithToStatus v = do
  v' <- v
  pure $ case v' of
    Left err -> err
    Right _ -> "Success"

server ::
  JWK ->
  Config ->
  (forall a. SqlBack a -> IO a) ->
  Application
server jwk conf runer =
  let jwtSettings = defaultJWTSettings jwk
      cookieSettings = defaultCookieSettings
      setCookie = acceptLogin cookieSettings jwtSettings
   in genericServeTWithContext
        liftIO
        Api
          { _auth =
              Auth
                { _register = \req -> do
                    let func = register (_authConfig conf) setCookie req
                    let res = runer $ runExceptT func
                    eithToStatus res
                , _signIn = \req -> do
                    let func = signIn (_authConfig conf) setCookie req
                    res <- runer $ runExceptT func
                    case res of
                      Left err -> error err
                      Right v -> pure v
                }
          , _notes = \_ -> do
              undefined
          }
        (jwtSettings :. cookieSettings :. EmptyContext)
