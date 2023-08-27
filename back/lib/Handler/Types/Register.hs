{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Types.Register where

import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Database.Beam.Postgres (Connection)
import Database.Entities.User (HashedPassword, Login, Salt, User)

data RegistrationError
  = InvalidRequest
  | LoginAlreadyTaken

data CreateUserData = CreateUserData
  { _createdAt :: UTCTime
  , _login :: Login
  , _hashedPassword :: HashedPassword
  , _salt :: Salt
  }

data Handle m = Handle
  { _addUser :: CreateUserData -> m (Maybe User)
  , _throw :: forall a. RegistrationError -> m a
  , _connection :: Connection
  , _currentTime :: m UTCTime
  , _hashPassword :: ByteString -> Salt -> HashedPassword
  , _generateSalt :: m Salt
  }
