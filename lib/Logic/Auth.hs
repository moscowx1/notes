{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Auth (Handle (..), signIn, register) where

import Control.Monad.Error.Class (MonadError)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (User (..))
import Dto.Auth (Credential (login, password), LoginReq, RegisterReq)
import Types (HashedPassword, Login, Password, Salt)

data Handle m = Handle
  { _generateSalt :: m Salt
  , _currentTime :: m UTCTime
  , _hashPassword :: Password -> Salt -> HashedPassword
  , _addToDb :: User -> m (Maybe User)
  , _getUser :: Login -> m (Maybe User)
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

register ::
  Monad m =>
  Handle m ->
  RegisterReq ->
  m (Maybe User)
register Handle{..} req = do
  let validCred = getValidCreds req
  case validCred of
    Left _ -> pure Nothing
    Right (ValidCred{..}) -> do
      salt <- _generateSalt
      curTime <- _currentTime
      let hashedPassword = _hashPassword (encodeUtf8 password) salt
      _addToDb $
        User
          { userLogin = login
          , userSalt = salt
          , userPassword = hashedPassword
          , userCreatedAt = curTime
          }

signIn ::
  MonadError err m =>
  Handle m ->
  LoginReq ->
  m (Maybe User)
signIn Handle{..} req = do
  let cred = getValidCreds req
  case cred of
    Left _ -> pure Nothing
    Right (ValidCred{..}) -> do
      mUser <- _getUser login
      case mUser of
        Nothing -> pure Nothing
        Just user -> do
          let password2 = _hashPassword (encodeUtf8 password) (userSalt user)
          if userPassword user == password2
            then pure $ Just user
            else pure Nothing
