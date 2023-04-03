{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Auth (
  Handle (..),
  register,
  signIn,
  JwtHeaderSetter,
) where

import Api (JwtHeader, Payload (..), Role (UserRole))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT, MonadIO (liftIO), MonadTrans (lift), when)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (User (..))
import Dto.Auth (Credential (login, password), LoginReq, RegisterReq)
import Servant.API (NoContent (NoContent))
import Types (HashedPassword, Login, Password, Salt)

type JwtHeaderSetter =
  Payload ->
  IO
    ( Maybe
        (NoContent -> JwtHeader)
    )

data Handle m = Handle
  { _generateSalt :: m Salt
  , _currentTime :: m UTCTime
  , _hashPassword :: Password -> Salt -> HashedPassword
  , _addToDb :: User -> m (Maybe User)
  , _getUser :: Login -> m (Maybe User)
  , _setCookie :: JwtHeaderSetter
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

liftMaybe :: Functor m => e -> m (Maybe a) -> ExceptT e m a
liftMaybe err = maybeToExceptT err . MaybeT

register ::
  (Monad m) =>
  Handle m ->
  RegisterReq ->
  ExceptT String m User
register Handle{..} req = do
  ValidCred{..} <- except $ getValidCreds req
  salt <- lift _generateSalt
  curTime <- lift _currentTime
  let hashedPassword = _hashPassword (encodeUtf8 password) salt
  liftMaybe "login already taken" $
    _addToDb
      User
        { userLogin = login
        , userSalt = salt
        , userPassword = hashedPassword
        , userCreatedAt = curTime
        }

signInErr :: String
signInErr = "login or password didn`t match"

signIn ::
  MonadIO m =>
  Handle m ->
  LoginReq ->
  ExceptT String m JwtHeader
signIn Handle{..} req = do
  ValidCred{..} <- except $ getValidCreds req
  user <- liftMaybe signInErr $ _getUser login
  let password2 = _hashPassword (encodeUtf8 password) (userSalt user)
  when (userPassword user /= password2) (throwError signInErr)
  let paylod = Payload{role = UserRole, login = userLogin user}
  res <- liftMaybe "error settings cookie" $ liftIO $ _setCookie paylod
  pure $ res NoContent
