{-# LANGUAGE FlexibleContexts #-}

module Handler.Handler.Register where

import Api (JwtHeader, Payload)
import qualified Config.Auth as CA
import qualified Config.Config as CG
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Pool (Pool, withResource)
import Data.Time (getCurrentTime)
import Database.Beam.Postgres (Connection)
import Dto.Auth (Credential)
import Handler.DataAccess.Register (addUser)
import qualified Handler.Logic.Register as L
import Handler.Types.Register (Handle (..), RegistrationError (..))
import Servant (
  Handler (Handler),
  NoContent (NoContent),
  ServerError (errReasonPhrase),
  err400,
  err409,
  err500,
 )

type SetCookie = (Payload -> IO (Maybe (NoContent -> JwtHeader)))

mapError :: RegistrationError -> ServerError
mapError InvalidRequest = err400{errReasonPhrase = "Invalid request"}
mapError LoginAlreadyTaken = err409{errReasonPhrase = "Login already taken"}
mapError ErrorSettingCookie = err500{errReasonPhrase = "Error setting cookie"}

register ::
  Pool Connection ->
  SetCookie ->
  CG.Config ->
  Credential ->
  Handler JwtHeader
register pool setCookie config request =
  let setCookie' :: Payload -> IO (Maybe JwtHeader)
      setCookie' p = ((\f -> f NoContent) <$>) <$> setCookie p
      handle' connection =
        Handle
          { _addUser = liftIO . addUser connection
          , _throw = throwError . mapError
          , _currentTime = liftIO getCurrentTime
          , _hashPassword =
              fastPBKDF2_SHA512 $
                Parameters
                  { iterCounts = CA._generatingIterCount $ CG._auth config
                  , outputLength = CA._hashedPasswordLength $ CG._auth config
                  }
          , _generateSalt = liftIO $ getEntropy $ CA._saltLength $ CG._auth config
          , _setCookie = liftIO . setCookie'
          }
   in Handler $ ExceptT $ withResource pool (\connection -> runExceptT $ L.register (handle' connection) request)
