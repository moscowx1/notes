{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Auth2 (signIn) where

import Api (JwtHeader)
import Config.Auth (Config (..))
import Control.Monad.Except (ExceptT, MonadIO (liftIO), MonadTrans (lift), runExceptT)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth2 (addUser, userByLogin)
import Database.Esqueleto.Experimental (SqlBackend, SqlPersistT)
import Dto.Auth (LoginReq, RegisterReq)
import qualified Logic.Auth as LA
import Logic.Auth2 (Handle2 (..), JwtHeaderSetter)
import Servant (ServerError)
import Types (SqlBack, SqlBackT)

handle ::
  (MonadIO m) =>
  Config ->
  JwtHeaderSetter m ->
  Handle2 (SqlPersistT (MaybeT m))
handle Config {..} setCookie =
  Handle
    { _generateSalt = liftIO $ getEntropy _saltLength,
      _currentTime = liftIO getCurrentTime,
      _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = _generatingIterCount,
              outputLength = _hashedPasswordLength
            },
      _addToDb = addUser,
      _getUser = userByLogin,
      _setCookie = lift . lift . setCookie
    }

signIn ::
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  SqlBackT (ExceptT ServerError IO) JwtHeader
signIn config jwtSetter = LA.signIn h
  where
    h = handle config (jwtSetter)
