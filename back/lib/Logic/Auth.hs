{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Auth (
  AuthError (..),
  Handle (..),
  signIn,
  register,
  JwtSetter,
)
where

import Api (Payload (..), Role (UserRole))
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (HashedPassword, Login, Salt, User (..))
import Database.Esqueleto.Experimental (Entity (entityKey, entityVal), fromSqlKey)
import Dto.Auth (Credential (..), LoginReq, RegisterReq)
import Handle.Logger (_logDebug, _logError, _logInfo)
import qualified Handle.Logger as Logger

type JwtSetter m a = Payload -> m (Maybe a)

data AuthError
  = InvalidLogin
  | InvalidPassword
  | LoginAlreadyTaken
  | ErrorSettingCookie
  | WrongPassword
  | WrongLogin
  deriving (Show, Eq)

data Handle m r = Handle
  { _generateSalt :: m Salt
  , _currentTime :: m UTCTime
  , _hashPassword :: Password -> Salt -> HashedPassword
  , _addToDb :: User -> m (Maybe (Entity User))
  , _getUser :: Login -> m (Maybe (Entity User))
  , _authenticate :: JwtSetter m r
  , _throw :: forall a. AuthError -> m a
  , _logger :: Logger.Handle m
  }

type Password = ByteString

data ValidCred = ValidCred
  { login :: Text
  , password :: Text
  }

creds :: (Monad m) => Handle m r -> Credential -> m ValidCred
creds Handle{..} Credential{..} = do
  _logDebug _logger "validating user credentials"

  when
    (T.length login < 5)
    (_logError _logger "invalid user login" >> _throw InvalidLogin)

  when
    (T.length password < 5)
    (_logError _logger "invalid user password" >> _throw InvalidPassword)

  pure ValidCred{..}

generateSalt :: (Monad m) => Handle m r -> m Salt
generateSalt Handle{..} = do
  _logInfo _logger "generating salt"
  _generateSalt

getTime :: (Monad m) => Handle m r -> m UTCTime
getTime Handle{..} = do
  _logDebug _logger "getting current time"
  _currentTime

hashPwd ::
  (Monad m) =>
  Handle m r ->
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
  Handle m r ->
  User ->
  m (Entity User)
addToDb Handle{..} u = do
  _logInfo _logger "adding user to database"
  _addToDb u >>= \case
    Nothing -> do
      _logError _logger "login already taken"
      _throw LoginAlreadyTaken
    Just x -> pure x

authenticate ::
  (Monad m) =>
  Handle m r ->
  Entity User ->
  m r
authenticate Handle{..} user = do
  _logDebug _logger "setting cookie"
  let payload =
        Payload
          { role = UserRole
          , login = userLogin $ entityVal user
          , userId = fromSqlKey $ entityKey user
          }
  _authenticate payload >>= \case
    Nothing -> _throw ErrorSettingCookie
    Just x -> pure x

register ::
  (Monad m) =>
  Handle m r ->
  RegisterReq ->
  m r
register h req = do
  ValidCred{..} <- creds h req
  userSalt <- generateSalt h
  userCreatedAt <- getTime h
  userPassword <- hashPwd h password userSalt
  let userLogin = login
  let user = User{..}
  addToDb h user >>= authenticate h

getUser ::
  (Monad m) =>
  Handle m r ->
  Login ->
  m (Entity User)
getUser Handle{..} login = do
  _logInfo _logger "getting user by login"
  _getUser login >>= \case
    Nothing -> do
      _logError _logger $ "user with login '" <> login <> "' not found"
      _throw WrongLogin
    Just x -> pure x

signIn ::
  (Monad m) =>
  Handle m r ->
  LoginReq ->
  m r
signIn h@Handle{..} req = do
  ValidCred{..} <- creds h req
  entityUser <- getUser h login
  let user = entityVal entityUser
  pwdToCheck <- hashPwd h password (userSalt user)
  when
    (userPassword user /= pwdToCheck)
    (_logError _logger "password did't matched" >> _throw WrongPassword)
  authenticate h entityUser
