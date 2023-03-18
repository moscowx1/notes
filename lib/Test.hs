{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Crypto.Random.Entropy (getEntropy)
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA512, Parameters (..))
import Data.Text (Text, unpack)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Config (Config(..))
import Utils (throwLeft)
import Data.Aeson (eitherDecodeFileStrict)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import Database.Persist.Postgresql
  ( withPostgresqlPool
  , runSqlPool
  , SqlBackend
  )
import Database.Esqueleto.Experimental ((^.), (==.), (&&.))
import qualified Database.Esqueleto.Experimental as E
import Lib (User (..), EntityField (UserLogin, UserPassword, UserSalt))
import Control.Monad.Reader (ReaderT, MonadIO (liftIO), guard, MonadTrans (lift), void)
import Data.Time (getCurrentTime, UTCTime)
import Debug.Trace (traceShow)

password :: ByteString
password = "Asasdfbgzxc;lnmksda"

loginText :: Text
loginText = "santrsasda"

initHandle = Handle
  { generatingIterCount = 8192
  , hashedPasswordLength = 64
  , saltLength = 32
  }

data Handle = Handle
  { generatingIterCount :: Int
  , hashedPasswordLength :: Int
  , saltLength :: Int
  }

type Password = ByteString
type Salt = ByteString
type Login = Text
type HashedPassword = ByteString

getHashedPwd :: Handle -> Password -> Salt -> HashedPassword
getHashedPwd Handle{..} = fastPBKDF2_SHA512 prms
  where
    prms = Parameters
      { iterCounts = generatingIterCount
      , outputLength = hashedPasswordLength
      }

createUser :: MonadIO m => Handle -> Login -> Password -> m User
createUser h@Handle{..} login pwd = do
  (salt :: ByteString) <- liftIO $ getEntropy saltLength
  curTime <- liftIO getCurrentTime
  let pwdHashed = getHashedPwd h pwd salt
  pure $ User
    { userLogin = login
    , userSalt = salt
    , userPassword = pwdHashed
    , userCreatedAt = curTime
    }

register 
  :: MonadIO m 
  => Handle 
  -> Login
  -> Password
  -> ReaderT SqlBackend m Bool
register h login password = do
  user <- liftIO $ createUser h login password
  res <- E.insertUniqueEntity user
  pure $ case res of
    Nothing -> False
    Just _ -> True

userByLogin 
  :: (MonadIO m) 
  => Text 
  -> ReaderT SqlBackend m (Maybe (E.Entity User))
userByLogin login = E.selectOne $ do
  user <- E.from $ E.table @User
  E.where_ $ user ^. UserLogin ==. E.val login
  pure user

login 
  :: (MonadIO m) 
  => Handle 
  -> Login 
  -> Password 
  -> ReaderT SqlBackend m Bool
login h login password = do
  user <- userByLogin login
  let hashed = getHashedPwd h password 
  pure True

truncateUsers :: ReaderT SqlBackend (NoLoggingT IO) ()
truncateUsers = E.rawExecute "truncate table \"user\" cascade;" []

main :: IO ()
main = do
  Config {..} <- throwLeft <$> eitherDecodeFileStrict "config.json"
  runNoLoggingT do
    withPostgresqlPool 
      (encodeUtf8 _connectionString) 
      _poolConnections \pool -> do 
        runSqlPool truncateUsers pool
      --runSqlPool (void insertUser2) pool
      --runSqlPool (void insertUser) pool
      --runSqlPool login' pool
      --runSqlPool getAll_ pool