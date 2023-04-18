{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Auth (register, signIn) where

import Api (JwtHeader)
import Config.Auth (Config (..))
import Control.Monad.Except (
  ExceptT (..),
  MonadIO (liftIO),
  MonadTrans (lift),
  withExceptT,
 )
import Control.Monad.Logger (LoggingT (LoggingT))
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth (addUser, userByLogin)
import Dto.Auth (LoginReq, RegisterReq)
import Logger (runAppLoggingT)
import Logic.Auth (AuthError (..), JwtHeaderSetter)
import qualified Logic.Auth as LA
import Servant (Handler (Handler), ServerError (errReasonPhrase), err400, err401, err409, err500)
import Types (SqlRuner)

handle ::
  SqlRuner ->
  Config ->
  LA.JwtHeaderSetter IO ->
  LA.Handle (ExceptT AuthError IO)
handle sql Config{..} setCookie =
  LA.Handle
    { _generateSalt = liftIO $ getEntropy _saltLength
    , _currentTime = liftIO getCurrentTime
    , _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = _generatingIterCount
            , outputLength = _hashedPasswordLength
            }
    , _addToDb = lift . sql . addUser
    , _getUser = lift . sql . userByLogin
    , _setCookie = liftIO . setCookie
    }

handle' sql Config{..} setCookie =
  LA.Handle
    { _generateSalt = liftIO $ getEntropy _saltLength
    , _currentTime = liftIO getCurrentTime
    , _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = _generatingIterCount
            , outputLength = _hashedPasswordLength
            }
    , _addToDb = lift . sql . addUser
    , _getUser = lift . sql . userByLogin
    , _setCookie = liftIO . setCookie
    }

withPhrase :: ServerError -> String -> ServerError
withPhrase err phrase = err{errReasonPhrase = phrase}

mapper :: LA.AuthError -> ServerError
mapper = \case
  InvalidLogin -> withPhrase err401 "invalid login"
  InvalidPassword -> withPhrase err401 "invalid password"
  LoginAlreadyTaken -> withPhrase err409 "login already taken"
  CannotAuth -> withPhrase err400 "login or password didn`t matched"
  ErrorSettingCookie -> err500

signIn ::
  SqlRuner ->
  FilePath ->
  Config ->
  LA.JwtHeaderSetter IO ->
  LoginReq ->
  Handler JwtHeader
signIn runer fp config jwtSetter = Handler . z'
 where
  z' r = runReaderT (lift h') $ withExceptT mapper $ runAppLoggingT fp (LA.signIn r)

  h' = handle' runer config jwtSetter

-- h = handle' runer config jwtSetter

register ::
  SqlRuner ->
  Config ->
  JwtHeaderSetter IO ->
  RegisterReq ->
  Handler JwtHeader
register runer config jwtSet = Handler . register'
 where
  h = handle runer config jwtSet
  register' r = withExceptT mapper (LA.register h r)

register' ::
  SqlRuner ->
  Config ->
  JwtHeaderSetter IO ->
  RegisterReq ->
  Handler JwtHeader
register' runer config jwtSet = Handler . register'
 where
  h = handle runer config jwtSet
  register' r = withExceptT mapper (LA.register h r)
