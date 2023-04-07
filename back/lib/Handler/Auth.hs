{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Auth (register, signIn, signIn') where

import Api (JwtHeader)
import Config.Auth (Config (..))
import Control.Monad.Except (ExceptT, MonadIO (liftIO), MonadTrans (lift), runExceptT)
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
import Servant (ServerError)
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

register ::
  MonadIO m =>
  Config ->
  JwtHeaderSetter m ->
  RegisterReq ->
  ExceptT String (SqlBackT m) JwtHeader
register _ = undefined -- LA.register . handle c

{-
signIn ::
  MonadIO m =>
  Config ->
  JwtHeaderSetter ->
  LoginReq ->
  ExceptT String (SqlBackT m) JwtHeader
signIn c = LA.signIn . handle c
-}

{-
Expected: Handle
              (ReaderT
                 SqlBackend
                 (NoLoggingT (ExceptT ServerError IO)))
    Actual: Handle (SqlBackT (ExceptT ServerError m))
-}
signIn ::
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  SqlBackT (ExceptT ServerError IO) JwtHeader
signIn config jwtSetter = LA.signIn (handle config (lift . jwtSetter))

signIn' ::
  Config ->
  JwtHeaderSetter IO ->
  LoginReq ->
  SqlBackend ->
  IO (Either ServerError JwtHeader)
signIn' c j r = \b -> runExceptT $ runNoLoggingT $ runReaderT kek b
 where
  kek = signIn c j r
