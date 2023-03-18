{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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

prm :: Parameters
prm = Parameters { iterCounts = 8192, outputLength = 64 }

t3 :: ByteString -> IO (ByteString, ByteString)
t3 pwd = let salt = getEntropy 32
  in (,) <$> salt <*> (fastPBKDF2_SHA512 prm pwd <$> salt)

-- working
t :: ByteString -> IO Bool
t pwd = do
  ss <- getEntropy 32 :: IO ByteString
  let k1  = fastPBKDF2_SHA512 prm pwd ss :: ByteString
  let k2 =  fastPBKDF2_SHA512 prm pwd ss
  pure $ k1 == k2
-- not working
k :: ByteString -> IO Bool
k pwd = do
  (salt, hp1) <- t3 pwd
  let hp2 = fastPBKDF2_SHA512 prm pwd salt
  pure $ hp1 == hp2

createUser
  :: (ReaderT SqlBackend (NoLoggingT IO) a -> NoLoggingT IO a)
  -> Text
  -> Text
  -> User
createUser _ _ _ = do
  undefined

getAll_ :: ReaderT SqlBackend (NoLoggingT IO) ()
getAll_ = do
  us <- getAll
  liftIO $ print us
  pure ()

getAll :: ReaderT SqlBackend (NoLoggingT IO) [E.Entity User]
getAll = E.select $ E.from $ E.table @User

password :: ByteString
password = "Asasdfbgzxc;lnmksda"

loginText :: Text
loginText = "santrsasda"

insertUser2 = insertUser2' loginText password

insertUser2' :: Text -> ByteString -> ReaderT SqlBackend (NoLoggingT IO) (E.Key User)
insertUser2' login pwd = do
  salt <- liftIO (getEntropy 32 :: IO ByteString)
  let pwdHashed = fastPBKDF2_SHA512 prm pwd salt
  curTime <- liftIO getCurrentTime
  E.insert $ User
    { userLogin = login
    , userSalt = salt
    , userPassword = pwdHashed
    , userCreatedAt = curTime
    }

userByLogin :: Text -> ReaderT SqlBackend (NoLoggingT IO) (Maybe (E.Entity User))
userByLogin login = E.selectOne $ do
  user <- E.from $ E.table @User
  E.where_ $ user ^. UserLogin ==. E.val login
  pure user

login :: Text -> ByteString -> ReaderT SqlBackend (NoLoggingT IO) (Maybe (E.Entity User))
login login password = do
  Just user <- userByLogin login
  let hashedPwd = fastPBKDF2_SHA512 prm password (userSalt $ E.entityVal user)
  pure $ if hashedPwd == userPassword (E.entityVal user)
    then Just user
    else Nothing

login' :: ReaderT SqlBackend (NoLoggingT IO) ()
login' = do
  u <- login loginText password
  liftIO $ print u
  pure ()

truncateUsers :: ReaderT SqlBackend (NoLoggingT IO) ()
truncateUsers = E.rawExecute "truncate table \"user\" cascade;" []

main :: IO ()
main = do
  Config {..} <- throwLeft <$> eitherDecodeFileStrict "config.json"
  runNoLoggingT do
    withPostgresqlPool (encodeUtf8 _connectionString) _poolConnections \pool -> do
      --runSqlPool truncateUsers pool
      --runSqlPool (void insertUser2) pool
      --runSqlPool (void insertUser) pool
      runSqlPool login' pool
      --runSqlPool getAll_ pool