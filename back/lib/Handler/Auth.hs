{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Auth (register, signIn) where

import Api (JwtHeader)
import Config.Auth (Config (..))
import Control.Monad.Except (ExceptT (..), MonadIO (liftIO), MonadTrans (lift), runExceptT)
import Control.Monad.Identity
import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Control.Monad.Reader (ReaderT (runReaderT))
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth (addUser, userByLogin)
import Database.Esqueleto.Experimental (SqlBackend)
import Dto.Auth (LoginReq, RegisterReq)
import Logic.Auth (Handle (..), JwtHeaderSetter)
import qualified Logic.Auth as LA
import Servant (Handler (Handler), ServerError)
import Types (SqlBack, SqlBackT)

handle :: (MonadIO m) => Config -> JwtHeaderSetter m -> Handle (SqlBackT m)
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
    , _setCookie = lift . lift . setCookie
    }

lol ::
  JwtHeaderSetter IO ->
  JwtHeaderSetter
    (ExceptT ServerError (ReaderT SqlBackend (NoLoggingT IO)))
lol set = liftIO . set

handle2 ::
  Config ->
  JwtHeaderSetter IO ->
  Handle (ExceptT ServerError SqlBack)
handle2 Config{..} setCookie =
  Handle
    { _generateSalt = liftIO $ getEntropy _saltLength
    , _currentTime = liftIO getCurrentTime
    , _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = _generatingIterCount
            , outputLength = _hashedPasswordLength
            }
    , _addToDb = lift . addUser
    , _getUser = lift . userByLogin
    , _setCookie = liftIO . setCookie
    }

handle3 ::
  (forall a. SqlBack a -> IO a) ->
  Config ->
  JwtHeaderSetter IO ->
  Handle (ExceptT ServerError IO)
handle3 sql Config{..} setCookie =
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

signIn3 ::
  (forall a. SqlBack a -> IO a) ->
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  Handler JwtHeader
signIn3 sql config jwtSetter = LA.signIn h
 where
  h = handle3 sql config jwtSetter
  h' = Handler h

register ::
  MonadIO m =>
  Config ->
  JwtHeaderSetter m ->
  RegisterReq ->
  ExceptT String (SqlBackT m) JwtHeader
register _ = undefined -- LA.register . handle c

signIn ::
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  -- Handler (SqlBack JwtHeader)
  ExceptT ServerError SqlBack JwtHeader
signIn config jwtSetter = LA.signIn (handle2 config jwtSetter)

signIn2 ::
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  -- Handler (SqlBack JwtHeader)
  ExceptT ServerError SqlBack JwtHeader
signIn2 config jwtSetter = LA.signIn (handle2 config jwtSetter)
