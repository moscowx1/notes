{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Auth (
  AuthError (..),
  Handle (..),
  signIn,
  register,
  JwtHeaderSetter,
) where

import Api (JwtHeader, Payload (..), Role (UserRole))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (User (..), HashedPassword, Salt, Login)
import Dto.Auth (Credential (Credential, login, password), LoginReq, RegisterReq)
import Handle.Logger (_logDebug, _logError, _logInfo)
import qualified Handle.Logger as Logger
import Servant.API (NoContent (NoContent))
import Data.ByteString (ByteString)

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
  , _throw :: forall a. AuthError -> m a -- TODO: checkout better solution
  , _logger :: Logger.Handle m
  }

type Password = ByteString

data ValidCred = ValidCred
  { login :: Text
  , password :: Text
  }

creds :: (Monad m) => Handle m -> Credential -> m ValidCred
creds Handle{..} Credential{..} = do
  _logDebug _logger "validating user credentials"

  when
    (T.length login <= 3)
    (_logError _logger "invalid user login" >> _throw InvalidLogin)

  when
    (T.length password <= 5)
    (_logError _logger "invalid user password" >> _throw InvalidPassword)

  pure ValidCred{..}

generateSalt :: (Monad m) => Handle m -> m Salt
generateSalt Handle{..} = do
  _logInfo _logger "generating salt"
  _generateSalt

getTime :: (Monad m) => Handle m -> m UTCTime
getTime Handle{..} = do
  _logDebug _logger "getting current time"
  _currentTime

hashPwd ::
  (Monad m) =>
  Handle m ->
  Text ->
  Salt ->
  m HashedPassword
hashPwd Handle{..} pwd salt = do
  _logDebug _logger "converting password to bs"
  let pwdBS = encodeUtf8 pwd
  _logDebug _logger "hashing input password"
  pure $ _hashPassword pwdBS salt

addToDb ::
  (Monad m) =>
  Handle m ->
  User ->
  m User
addToDb Handle{..} u = do
  _logInfo _logger "adding user to database"
  _addToDb u >>= \case
    Nothing -> do
      _logError _logger "error adding user to database"
      _throw LoginAlreadyTaken
    Just x -> pure x

setCookie ::
  (Monad m) =>
  Handle m ->
  User ->
  m JwtHeader
setCookie Handle{..} user = do
  _logDebug _logger "setting cookie"
  let payload = Payload{role = UserRole, login = userLogin user}
  _setCookie payload >>= \case
    Nothing -> _throw ErrorSettingCookie
    Just c -> pure $ c NoContent

register ::
  (Monad m) =>
  Handle m ->
  RegisterReq ->
  m JwtHeader
register h req = do
  ValidCred{..} <- creds h req
  userSalt <- generateSalt h
  userCreatedAt <- getTime h
  userPassword <- hashPwd h password userSalt
  let userLogin = login
  let user = User{..}
  addToDb h user >>= setCookie h

getUser ::
  (Monad m) =>
  Handle m ->
  Login ->
  m User
getUser Handle{..} login = do
  _logInfo _logger "getting user by login"
  _getUser login >>= \case
    Nothing -> do
      _logError _logger $ "user with login '" <> login <> "' not found"
      _throw CannotAuth
    Just x -> pure x

signIn ::
  (Monad m) =>
  Handle m ->
  LoginReq ->
  m JwtHeader
signIn h@Handle{..} req = do
  ValidCred{..} <- creds h req
  user <- getUser h login
  pwdToCheck <- hashPwd h password (userSalt user)
  when
    (userPassword user /= pwdToCheck)
    (_logError _logger "password did't matched" >> _throw CannotAuth)
  setCookie h user
