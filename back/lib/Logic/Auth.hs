{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (User (..))
import Dto.Auth (Credential (login, password), LoginReq, RegisterReq)
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
  (MonadError AuthError m) =>
  JwtHeaderSetter m ->
  User ->
  m JwtHeader
setCookie cookieSetter user = do
  let payload = Payload{role = UserRole, login = userLogin user}
  cookieSetter payload >>= \case
    Nothing -> throwError ErrorSettingCookie
    Just c -> pure $ c NoContent

liftMaybe :: (MonadError e m) => e -> m (Maybe a) -> m a
liftMaybe err r = do
  r' <- r
  case r' of
    Nothing -> throwError err
    Just x -> pure x

register ::
  (MonadError AuthError m) =>
  Handle m ->
  RegisterReq ->
  m JwtHeader
register Handle{..} req = do
  ValidCred{..} <- liftEither $ getValidCreds req
  salt <- _generateSalt
  curTime <- _currentTime
  let hashedPassword = _hashPassword (encodeUtf8 password) salt
  let user =
        User
          { userLogin = login
          , userSalt = salt
          , userPassword = hashedPassword
          , userCreatedAt = curTime
          }
  _ <- liftMaybe LoginAlreadyTaken $ _addToDb user
  setCookie _setCookie user

signIn ::
  (MonadError AuthError m) =>
  Handle m ->
  LoginReq ->
  m JwtHeader
signIn Handle{..} req = do
  ValidCred{..} <- liftEither $ getValidCreds req
  user <- liftMaybe CannotAuth $ _getUser login
  let passwordToCheck = _hashPassword (encodeUtf8 password) (userSalt user)
  when (userPassword user /= passwordToCheck) (throwError CannotAuth)
  setCookie _setCookie user
