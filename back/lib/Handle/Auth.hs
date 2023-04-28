{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handle.Auth (register, signIn) where

import Api (JwtHeader)
import Config.Auth (Config (..))
import Control.Monad.Except (
  ExceptT (..),
  MonadError (throwError),
  MonadIO (liftIO),
  MonadTrans (lift),
  withExceptT,
 )
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth (addUser, userByLogin)
import Dto.Auth (LoginReq, RegisterReq)
import qualified Handle.Logger as Logger
import Logic.Auth (AuthError (..), JwtHeaderSetter)
import qualified Logic.Auth as LA
import Servant (Handler (Handler), ServerError (errReasonPhrase), err400, err401, err409, err500)
import Types (SqlRuner)

handle ::
  SqlRuner ->
  Logger.Handle (ExceptT AuthError IO) ->
  Config ->
  LA.JwtHeaderSetter IO ->
  LA.Handle (ExceptT AuthError IO)
handle sql l Config{..} setCookie =
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
    , _throw = throwError
    , _logger = l
    }

withPhrase :: ServerError -> String -> ServerError
withPhrase err phrase = err{errReasonPhrase = phrase}

mapper :: LA.AuthError -> ServerError
mapper a = case a of
  InvalidLogin -> withPhrase err401 "invalid login"
  InvalidPassword -> withPhrase err401 "invalid password"
  LoginAlreadyTaken -> withPhrase err409 "login already taken"
  ErrorSettingCookie -> err500
  WrongPassword -> e400
  UserNotFound -> e400
 where
  e400 = withPhrase err400 "login or password didn`t matched"

signIn ::
  SqlRuner ->
  Logger.Handle (ExceptT AuthError IO) ->
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  Handler JwtHeader
signIn r l c jwtSet = Handler . reg'
 where
  h = handle r l c jwtSet
  reg' = withExceptT mapper . LA.signIn h

register ::
  SqlRuner ->
  Logger.Handle (ExceptT AuthError IO) ->
  Config ->
  JwtHeaderSetter IO ->
  RegisterReq ->
  Handler JwtHeader
register r l c jwtSet = Handler . reg'
 where
  h = handle r l c jwtSet
  reg' = withExceptT mapper . LA.register h
