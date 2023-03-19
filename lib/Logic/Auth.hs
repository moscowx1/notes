{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Logic.Auth (Config(..), Handle(..), login, register) where

import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA512, Parameters (..))
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON, fieldLabelModifier, constructorTagModifier)
import Data.Time (UTCTime)
import DataAccess.Data (User(..))
import Types (Salt, Login, Password, HashedPassword)

data Config = Config
  { _generatingIterCount :: Int
  , _hashedPasswordLength :: Int
  , _saltLength :: Int
  } deriving (Eq, Show)

deriveJSON defaultOptions 
  { fieldLabelModifier = drop 1
  , constructorTagModifier = drop 1 
  } ''Config

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
getHashedPwd Handle { _config = Config {..} } password salt = pure hashedPwd
  where
    hashedPwd :: HashedPassword
    hashedPwd = fastPBKDF2_SHA512 prms password salt
    prms :: Parameters
    prms = Parameters
      { iterCounts = _generatingIterCount
      , outputLength = _hashedPasswordLength
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
  :: (Monad m)
  => Handle m
  -> Login
  -> Password
  -> m (Maybe User)
login h login' password = do
  mUser <- _getUser h login'
  case mUser of
    Nothing -> pure Nothing
    Just user -> do
      password2 <- getHashedPwd h password (userSalt user)
      if userPassword user == password2
        then pure $ Just user
        else pure Nothing
