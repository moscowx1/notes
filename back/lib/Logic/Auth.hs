{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Logic.Auth (
  AuthError (..),
  Handle (..),
  signIn,
  register,
  JwtHeaderSetter,
) where

import Api (JwtHeader, Payload (..), Role (UserRole))
import Control.Monad (join, when)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN, logInfoN)
import Control.Monad.Reader (MonadReader, MonadTrans (lift), ReaderT (ReaderT, runReaderT), asks)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (User (..))
import Dto.Auth (Credential (Credential, login, password), LoginReq, RegisterReq)
import Servant.API (NoContent (NoContent))
import Types (HashedPassword, Login, Password, Salt)

type JwtHeaderSetter m =
  Payload ->
  m
    ( Maybe
        (NoContent -> JwtHeader)
    )

data AuthError
  = InvalidLogin
  | InvalidPassword
  | LoginAlreadyTaken
  | CannotAuth
  | ErrorSettingCookie

data Handle m = Handle
  { _generateSalt :: m Salt
  , _currentTime :: m UTCTime
  , _hashPassword :: Password -> Salt -> HashedPassword
  , _addToDb :: User -> m (Maybe User)
  , _getUser :: Login -> m (Maybe User)
  , _setCookie :: JwtHeaderSetter m
  }

data ValidCred = ValidCred
  { login :: Text
  , password :: Text
  }

validateLogin :: Text -> Either AuthError Login
validateLogin login' =
  if T.length login' > 3
    then Right login'
    else Left InvalidLogin

validatePassword :: Text -> Either AuthError Text
validatePassword password =
  if T.length password > 5
    then Right password
    else Left InvalidPassword

getValidCreds :: Credential -> Either AuthError ValidCred
getValidCreds cred = do
  l <- validateLogin (Dto.Auth.login cred)
  p <- validatePassword (Dto.Auth.password cred)
  pure ValidCred{login = l, password = p}

setCookie ::
  JwtHeaderSetter m ->
  User ->
  Logic m JwtHeader
setCookie cookieSetter user = do
  let payload = Payload{role = UserRole, login = userLogin user}
  lift $ cookieSetter payload >>= \case
    Nothing -> throwError ErrorSettingCookie
    Just c -> pure $ c NoContent

liftMaybe :: (MonadError e m) => e -> m (Maybe a) -> m a
liftMaybe err r = do
  r' <- r
  case r' of
    Nothing -> throwError err
    Just x -> pure x

type Logic m a = (MonadLogger m, MonadError AuthError m) => ReaderT (Handle m) m a

validateLogin' :: Text -> Logic m Login
validateLogin' login' =
  if T.length login' > 3
    then pure login'
    else do
      logErrorN "invalid login"
      throwError InvalidLogin

validatePassword' :: Text -> Logic m Text
validatePassword' password =
  if T.length password > 5
    then pure password
    else do
      logErrorN "invalid password"
      throwError InvalidPassword

getValidCreds' ::
  Credential ->
  (Logic m) ValidCred
getValidCreds' Credential{..} =
  validateLogin' login
    >> validatePassword' password
    >> pure ValidCred{..}

getUser ::
  Login ->
  Logic m User
getUser login = do
  logInfoN "getting user by login"
  getter <- lift $ asks _getUser
  res <- getter login
  case res of
    Nothing -> do
      logErrorN $ "user with login'" <> login <> "' not found"
      throwError CannotAuth
    Just x -> do
      pure x

hashPwd ::
  Text ->
  Salt ->
  (Logic m) HashedPassword
hashPwd pwd salt = do
  logDebugN "hashing input password"
  hashAlg <- asks _hashPassword
  logInfoN "converting password to bs"
  let pwdBS = encodeUtf8 pwd
  pure $ hashAlg pwdBS salt

setCookie' :: User -> Logic m JwtHeader
setCookie' user = do
  let payload = Payload{role = UserRole, login = userLogin user}
  setter <- lift $ asks _setCookie
  setter payload >>= \case
    Nothing -> throwError ErrorSettingCookie
    Just c -> pure $ c NoContent

generateSalt :: Logic m Salt
generateSalt = do
  logDebugN "generating salt"
  join $ asks _generateSalt

getTime :: Logic m UTCTime
getTime = do
  logDebugN "generating time"
  join $ lift $ asks _currentTime

addToDb :: User -> Logic m User
addToDb u = do
  logInfoN "adding user to db"
  lift $ asks _addToDb >>= ($ u) >>= \case
    Nothing -> do
      logErrorN "error adding user to db"
      throwError LoginAlreadyTaken
    Just x -> pure x

signIn ::
  LoginReq ->
  (Logic m) JwtHeader
signIn req = do
  logDebugN "validating user"
  ValidCred{..} <- getValidCreds' req
  user <- getUser login
  pwdToCheck <- hashPwd password (userSalt user)
  when
    (userPassword user /= pwdToCheck)
    (logErrorN "invalid password" >> throwError CannotAuth)
  setCookie' user

register ::
  RegisterReq ->
  (Logic m) JwtHeader
register req = do
  ValidCred{..} <- getValidCreds' req
  userSalt <- generateSalt
  userCreatedAt <- getTime
  userPassword <- hashPwd password userSalt
  let userLogin = login
  let user = User{..}
  addToDb user >>= setCookie'