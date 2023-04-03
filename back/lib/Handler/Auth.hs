{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Auth (register, signIn) where

import Api (JwtHeader)
import Config.Auth (Config (..))
import Control.Monad.Except (ExceptT, MonadIO (liftIO))
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth (addUser, userByLogin)
import DataAccess.Data (User)
import Dto.Auth (LoginReq, RegisterReq)
import Logic.Auth (Handle (..), JwtHeaderSetter)
import qualified Logic.Auth as LA
import Types (SqlBackT)

handle :: (MonadIO m) => Config -> JwtHeaderSetter -> Handle (SqlBackT m)
handle Config{..} setCookie =
  Handle
    { _generateSalt = liftIO $ getEntropy _saltLength
    , _currentTime = liftIO getCurrentTime
    , _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = _generatingIterCount
            , outputLength = _hashedPasswordLength
            }
    , _addToDb = addUser
    , _getUser = userByLogin
    , _setCookie = setCookie
    }

register ::
  MonadIO m =>
  Config ->
  JwtHeaderSetter ->
  RegisterReq ->
  ExceptT String (SqlBackT m) User
register c = LA.register . handle c

signIn ::
  MonadIO m =>
  Config ->
  JwtHeaderSetter ->
  LoginReq ->
  ExceptT String (SqlBackT m) JwtHeader
signIn c = LA.signIn . handle c
