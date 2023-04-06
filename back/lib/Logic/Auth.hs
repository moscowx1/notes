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
import Servant (ServerError (errReasonPhrase), err400, err401, err500)
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
  , _getUser :: Login -> m (Maybe User) -- TODO: try maybeT
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

setCookie ::
  (MonadIO m) =>
  JwtHeaderSetter ->
  User ->
  ExceptT String m JwtHeader
setCookie cookieSetter user = do
  let payload = Payload{role = UserRole, login = userLogin user}
  cookie <- liftMaybe "error setting cookie" $ liftIO $ cookieSetter payload
  pure $ cookie NoContent

register ::
  (MonadIO m) =>
  Handle m ->
  RegisterReq ->
  ExceptT String m JwtHeader
register Handle{..} req = do
  ValidCred{..} <- except $ getValidCreds req
  salt <- lift _generateSalt
  curTime <- lift _currentTime
  let hashedPassword = _hashPassword (encodeUtf8 password) salt
  let user =
        User
          { userLogin = login
          , userSalt = salt
          , userPassword = hashedPassword
          , userCreatedAt = curTime
          }
  _ <- liftMaybe "login already taken" $ _addToDb user
  setCookie _setCookie user

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
  (MonadIO m, MonadError ServerError m) =>
  JwtHeaderSetter ->
  User ->
  m JwtHeader
setCookie2 cookieSetter user = do
  let payload = Payload{role = UserRole, login = userLogin user}
  res <- liftIO $ cookieSetter payload
  case res of
    Nothing -> throwError err500
    Just c -> pure $ c NoContent

liftMaybe' :: (MonadError ServerError m) => ServerError ->  m (Maybe a) -> m a
liftMaybe' err r = do
  r' <- r
  case r' of
    Nothing -> throwError err
    Just x -> pure x

signIn ::
  (MonadIO m, MonadError ServerError m) =>
  Handle m ->
  LoginReq ->
  m JwtHeader
signIn Handle{..} req = do
  ValidCred{..} <- liftEither $ getValidCreds req
  user <- liftMaybe' badLoginOrPassword $ _getUser login
  let passwordToCheck = _hashPassword (encodeUtf8 password) (userSalt user)
  when (userPassword user /= passwordToCheck) (throwError badLoginOrPassword)
  setCookie2 _setCookie user
