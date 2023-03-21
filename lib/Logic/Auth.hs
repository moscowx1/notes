{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Auth (Handle (..), login, register) where

import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import DataAccess.Data (User (..))
import Dto (UnvalidatedCredential (..))
import Types (HashedPassword, Login, Password, Salt)

data Handle m = Handle
  { _generateSalt :: m Salt
  , _currentTime :: m UTCTime
  , _hashPassword :: Password -> Salt -> HashedPassword
  , _addToDb :: User -> m (Maybe User)
  , _getUser :: Login -> m (Maybe User)
  }

register ::
  Monad m =>
  Handle m ->
  UnvalidatedCredential ->
  m (Maybe User)
register Handle{..} UnvalidatedCredential{..} = do
  salt <- _generateSalt
  curTime <- _currentTime
  let hashedPassword = _hashPassword (encodeUtf8 _password) salt
  _addToDb $
    User
      { userLogin = _login
      , userSalt = salt
      , userPassword = hashedPassword
      , userCreatedAt = curTime
      }

login ::
  (Monad m) =>
  Handle m ->
  Login ->
  Password ->
  m (Maybe User)
login Handle{..} login' password = do
  mUser <- _getUser login'
  case mUser of
    Nothing -> pure Nothing
    Just user -> do
      let password2 = _hashPassword password (userSalt user)
      if userPassword user == password2
        then pure $ Just user
        else pure Nothing
