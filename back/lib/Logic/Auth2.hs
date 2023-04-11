{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Auth2 (
  Handle2 (..),
  signIn,
  JwtHeaderSetter,
) where

import Api (JwtHeader, Payload (..), Role (UserRole))
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT), maybeToExceptT)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (User (..))
import Dto.Auth (Credential (login, password), LoginReq)
import Servant (ServerError (errReasonPhrase), err400, err401, err500)
import Servant.API (NoContent (NoContent))
import Types (HashedPassword, Login, Password, Salt)

type JwtHeaderSetter m =
  Payload ->
  MaybeT m (NoContent -> JwtHeader)

data Handle2 m = Handle
  { _generateSalt :: m Salt
  , _currentTime :: m UTCTime
  , _hashPassword :: Password -> Salt -> HashedPassword
  , _addToDb :: User -> m (Maybe User)
  , _getUser :: Login -> m (Maybe User)
  , _setCookie :: JwtHeaderSetter m
  }

data ValidCred = ValidCred
  { login :: T.Text
  , password :: T.Text
  }

validateLogin :: T.Text -> Either String Login
validateLogin login' =
  if T.length login' > 3
    then Right login'
    else Left "invalid login"

validatePassword :: T.Text -> Either String T.Text
validatePassword password =
  if T.length password > 5
    then Right password
    else Left "invalid password"

getValidCreds :: Credential -> Either String ValidCred
getValidCreds cred = do
  l <- validateLogin (Dto.Auth.login cred)
  p <- validatePassword (Dto.Auth.password cred)
  pure ValidCred{login = l, password = p}

badLoginOrPassword :: ServerError
badLoginOrPassword =
  err401
    { errReasonPhrase = "login or password didn`t matched"
    }

liftEither :: (MonadError ServerError m) => Either String a -> m a
liftEither res = case res of
  Left err ->
    throwError
      err400
        { errReasonPhrase = err
        }
  Right a -> pure a

setCookie2 ::
  (MonadError ServerError m) =>
  JwtHeaderSetter m ->
  User ->
  m JwtHeader
setCookie2 cookieSetter user = do
  let payload = Payload{role = UserRole, login = userLogin user}
  res <- runMaybeT $ cookieSetter payload
  case res of
    Nothing -> throwError err500
    Just c -> pure $ c NoContent

liftMaybe' :: (MonadError ServerError m) => ServerError -> m (Maybe a) -> m a
liftMaybe' err r = do
  r' <- r
  case r' of
    Nothing -> throwError err
    Just x -> pure x

throwMaybe ::
  (MonadError ServerError m) =>
  ServerError ->
  MaybeT m User ->
  m User
throwMaybe err mu = do
  u <- runMaybeT mu
  case u of
    Nothing -> throwError err
    Just x -> pure x

signIn ::
  (MonadError ServerError m) =>
  Handle2 m ->
  LoginReq ->
  m JwtHeader
signIn Handle{..} req = do
  ValidCred{..} <- liftEither $ getValidCreds req
  user <- liftMaybe' badLoginOrPassword $ _getUser login
  let passwordToCheck = _hashPassword (encodeUtf8 password) (userSalt user)
  when (userPassword user /= passwordToCheck) (throwError badLoginOrPassword)
  setCookie2 _setCookie user
