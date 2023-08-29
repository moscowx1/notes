{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Api (Api (..), Auth (..))
import qualified Config.Config as GC
import qualified Config.Database as DC
import Cors (corsMiddleware)
import Crypto.JOSE (JWK)
import Data.Aeson (decodeFileStrict, eitherDecodeFileStrict)
import Data.Pool (Pool, PoolConfig, defaultPoolConfig, newPool)
import Database.Beam.Postgres (ConnectInfo (..), Connection, close, connect)
import Handler.Handler.Register (register)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Application, Context (EmptyContext, (:.)))
import Servant.Auth.Server (
  CookieSettings (..),
  IsSecure (Secure),
  SameSite (SameSiteStrict),
  acceptLogin,
  defaultCookieSettings,
  defaultJWTSettings,
 )
import Servant.Server.Generic (genericServeTWithContext)

readConfig :: IO GC.Config
readConfig =
  eitherDecodeFileStrict "config.json" >>= \case
    Left e -> error e
    Right x -> pure x

createPool ::
  GC.Config ->
  PoolConfig Connection
createPool config =
  defaultPoolConfig
    (connect connectInfo)
    close
    (DC._keepAliveUnusedConnection dbConfig)
    (DC._maxConnections dbConfig)
 where
  dbConfig = GC._database config
  connectInfo =
    ConnectInfo
      { connectHost = DC._host dbConfig
      , connectPort = DC._port dbConfig
      , connectUser = DC._user dbConfig
      , connectPassword = DC._password dbConfig
      , connectDatabase = DC._database dbConfig
      }

main :: IO ()
main = do
  config <- readConfig
  pool <- newPool $ createPool config
  jwk <-
    decodeFileStrict "jwk.json" >>= \case
      Nothing -> error "error reading jwk"
      Just x -> pure x
  let port = GC._port config
  run port (corsMiddleware . logStdoutDev $ server jwk config pool)

server ::
  JWK ->
  GC.Config ->
  Pool Connection ->
  Application
server jwk config pool =
  let jwtSettings = defaultJWTSettings jwk
      cookieSettings =
        defaultCookieSettings
          { cookieIsSecure = Secure
          , cookieSameSite = SameSiteStrict
          , cookieXsrfSetting = Nothing
          }
      setCookie = acceptLogin cookieSettings jwtSettings
   in -- TODO: add logger
      -- logger :: forall err. Handle (ExceptT err IO)
      -- logger = mkLogger $ _logFile conf
      genericServeTWithContext
        id
        Api
          { _auth =
              Auth
                { _register = register pool setCookie config
                }
          }
        (jwtSettings :. cookieSettings :. EmptyContext)
