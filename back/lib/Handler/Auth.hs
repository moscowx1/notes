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
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth (addUser, userByLogin)
import Dto.Auth (LoginReq, RegisterReq)
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
  Config ->
  LA.JwtHeaderSetter IO ->
  LoginReq ->
  Handler JwtHeader
signIn runer config jwtSetter = Handler . signIn'
 where
  h = handle runer config jwtSetter
  signIn' r = withExceptT mapper (LA.signIn h r)

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
