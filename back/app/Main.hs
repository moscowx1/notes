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

import Api (Api (..), Auth (..), Notes (..))
import Config.Global (Config (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Cors (corsMiddleware)
import Crypto.JOSE (JWK)
import Data.Aeson (decodeFileStrict, eitherDecodeFileStrict)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (
  runSqlPool,
  withPostgresqlPool,
 )
import Handle.Auth (register, signIn)

import Control.Monad.Except (ExceptT)
import Handle.Logger (Handle, mkLogger)
import Handle.Notes (createNote, getNote)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (
  Application,
  Context (EmptyContext, (:.)),
  IsSecure (Secure),
  err401,
  throwError,
 )
import Servant.Auth.Server (
  AuthResult (Authenticated),
  CookieSettings (..),
  SameSite (..),
  acceptLogin,
  defaultCookieSettings,
  defaultJWTSettings,
 )
import Servant.Server.Generic (genericServeTWithContext)
import Types (SqlRuner)

main :: IO ()
main = do
  c <-
    eitherDecodeFileStrict "config.json" >>= \case
      Left e -> error e
      Right x -> pure x
  print c
  jwk <-
    decodeFileStrict "jwk.json" >>= \case
      Nothing -> error "error reading jwk"
      Just x -> pure x
  getPool c \pool -> do
    runServer jwk c pool
 where
  getPool Config{..} =
    runNoLoggingT
      . withPostgresqlPool
        (encodeUtf8 _connectionString)
        _poolConnections

  runServer jwk c pool =
    liftIO $
      run
        (_port c)
        ( corsMiddleware . logStdoutDev $
            server
              jwk
              c
              ( runNoLoggingT
                  . (`runSqlPool` pool)
              )
        )

server ::
  JWK ->
  Config ->
  SqlRuner ->
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

      logger :: forall err. Handle (ExceptT err IO)
      logger = mkLogger $ _logFile conf
   in genericServeTWithContext
        id
        Api
          { _auth =
              Auth
                { _signIn = signIn runer logger (_authConfig conf) setCookie
                , _register = register runer logger (_authConfig conf) setCookie
                }
          , _notes =
              \pay -> do
                Notes
                  { _get = getNote runer logger
                  , _create = do
                      case pay of
                        Authenticated p -> createNote runer p logger
                        _ -> const $ throwError err401
                  }
          , _session = \case
              Authenticated p -> pure p
              _ -> throwError err401
          }
        (jwtSettings :. cookieSettings :. EmptyContext)
