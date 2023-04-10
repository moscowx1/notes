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

import Api (Api (..), Auth (..), JwtHeader, Notes (Notes, _get))
import Config.Global (Config (..))
import Control.Monad.Except (ExceptT (ExceptT), MonadTrans (lift), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Cors (corsMiddleware)
import Crypto.JOSE (JWK)
import Data.Aeson (decodeFileStrict, eitherDecodeFileStrict)
import Data.Text.Encoding (encodeUtf8)
import DataAccess.Data (migrateAll)
import Database.Persist.Postgresql (
  SqlBackend,
  runMigration,
  runSqlPool,
  withPostgresqlPool,
 )
import Handler.Auth (register, signIn, signIn')

import Control.Monad.Reader (MonadReader (ask), ReaderT)
import JwtSupport ()
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (
  Application,
  Context (EmptyContext, (:.)),
  Handler (Handler),
  IsSecure (Secure),
  NoContent (NoContent),
  ServerError (ServerError),
  runHandler,
 )
import Servant.Auth.Server (
  CookieSettings (..),
  SameSite (..),
  acceptLogin,
  defaultCookieSettings,
  defaultJWTSettings,
 )
import Servant.Server.Generic (genericServeTWithContext)
import Types (SqlBack, SqlBackT)
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

-- test ::
--  ReaderT SqlBackend (NoLoggingT IO) a ->
--  (SqlBackend -> IO a) ->
--  ReaderT SqlBackend (NoLoggingT IO) a
test b f = m
 where
  m = do
    sql <- lift ask
    s <- f sql
    s

k :: ExceptT ServerError IO a -> Handler a
k = Handler

q ::
  SqlBackT (ExceptT ServerError IO) JwtHeader ->
  (SqlBack a -> IO a) ->
  ExceptT ServerError IO JwtHeader
q f r = let b = ask 

server ::
  JWK ->
  Config ->
  (forall a. SqlBack a -> IO a) ->
  Application
server jwk conf runer =
  let jwtSettings = defaultJWTSettings jwk
      cookieSettings =
        defaultCookieSettings
          { cookieIsSecure = Secure
          , cookieSameSite = SameSiteStrict
          , cookieXsrfSetting = Nothing
          }
      setCookie = acceptLogin cookieSettings jwtSettings
   in genericServeTWithContext
        Handler
        Api
          { _auth =
              Auth
                { _signIn = \r -> do
                    let func = signIn (_authConfig conf) setCookie r
                    undefined
                }
          , _notes = \p -> do
              Notes
                { _get = undefined :: ExceptT ServerError IO String
                }
          }
        {-
        Api
          { _auth =
              Auth
                { {-
                    _register = \req -> do
                      let func = register (_authConfig conf) setCookie req
                      res <- runer $ runExceptT func
                      let r2 = undefined :: Either ServerError String
                      let r3 = undefined :: Handler String
                      -- eithToStatus res
                      r3
                  ,  -}
                  _signIn = \_ -> do
                    --let m = signIn' (_authConfig conf) setCookie req
                    undefined
                    -- let func = signIn (_authConfig conf) setCookie req
                    -- let k = runReaderT runer func
                    -- let z = runer func
                    -- undefined
                }
          , _notes = \payload -> do
              Notes
                { _get = do
                    print payload
                    pure "hi"
                }
          }
          -}
        (jwtSettings :. cookieSettings :. EmptyContext)
