{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Logic.Auth where

import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA512, Parameters (..))
import Data.ByteString (ByteString)
import Data.Time (UTCTime)
import Data.Text (Text)
import Lib (User(..))

type Password = ByteString
type Salt = ByteString
type Login = Text
type HashedPassword = ByteString

data Config = Config
  { generatingIterCount :: Int
  , hashedPasswordLength :: Int
  , saltLength :: Int
  }

data Handle m = Handle
  { _config :: Config
  , _generateSalt :: Config -> m Salt
  , _currentTime :: m UTCTime
  , _addToDb :: User -> m (Maybe User)
  , _getUser :: Login -> m (Maybe User)
  }

getHashedPwd
  :: Monad m
  => Handle m
  -> Password
  -> Salt
  -> m HashedPassword
getHashedPwd Handle { _config } password salt = pure hashedPwd
  where
    hashedPwd :: ByteString
    hashedPwd = fastPBKDF2_SHA512 prms password salt
    prms :: Parameters
    prms = Parameters
      { iterCounts = generatingIterCount _config
      , outputLength = hashedPasswordLength _config
      }

createUser
  :: Monad m
  => Handle m
  -> Login
  -> Password
  -> m User
createUser h@Handle {..} login' pwd = do
  salt <- _generateSalt _config
  curTime <- _currentTime
  hashedPassword <- getHashedPwd h pwd salt
  pure $ User
    { userLogin = login'
    , userSalt = salt
    , userPassword = hashedPassword
    , userCreatedAt = curTime
    }

register
  :: Monad m
  => Handle m
  -> Login
  -> Password
  -> m (Maybe User)
register h@Handle{..} login' password = do
  user <- createUser h login' password
  _addToDb user

login
  :: (Monad m, MonadFail m)
  => Handle m
  -> Login
  -> Password
  -> m (Maybe User)
login h login' password = do
  Just user <- _getUser h login'
  hashedToCheck <- getHashedPwd h password (userSalt user)
  pure $ if userPassword user == hashedToCheck 
    then Just user 
    else Nothing
