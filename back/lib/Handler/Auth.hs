{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Auth (register, signIn) where

import Api (JwtHeader)
import Config.Auth (Config (..))
import Control.Monad.Except (
  ExceptT (..),
  MonadIO (liftIO),
  MonadTrans (lift),
 )
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth (addUser, userByLogin)
import Dto.Auth (LoginReq, RegisterReq)
import Logic.Auth (Handle (..), JwtHeaderSetter)
import qualified Logic.Auth as LA
import Servant (Handler (Handler), ServerError)
import Types (SqlRuner)

handle ::
  SqlRuner ->
  Config ->
  JwtHeaderSetter IO ->
  Handle (ExceptT ServerError IO)
handle sql Config{..} setCookie =
  Handle
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

signIn ::
  SqlRuner ->
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  Handler JwtHeader
signIn runer config jwtSetter = Handler . LA.signIn h
 where
  h = handle runer config jwtSetter

register ::
  SqlRuner ->
  Config ->
  JwtHeaderSetter IO ->
  RegisterReq ->
  Handler JwtHeader
register runer config jwtSet = Handler . LA.register h
 where
  h = handle runer config jwtSet
