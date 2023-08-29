{-# LANGUAGE RankNTypes #-}

module Handler.Types.Register where

import Api (JwtSetter)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Database.Entities.User (HashedPassword, Login, Salt, User)

data RegistrationError
  = InvalidRequest
  | LoginAlreadyTaken
  | ErrorSettingCookie
  deriving (Eq, Show)

data CreateUserData = CreateUserData
  { _createdAt :: UTCTime
  , _login :: Login
  , _hashedPassword :: HashedPassword
  , _salt :: Salt
  }

data Handle m r = Handle
  { _addUser :: CreateUserData -> m (Maybe User)
  , _throw :: forall a. RegistrationError -> m a
  , _currentTime :: m UTCTime
  , _hashPassword :: ByteString -> Salt -> HashedPassword
  , _generateSalt :: m Salt
  , _setCookie :: JwtSetter m r
  }
