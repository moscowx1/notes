{-# LANGUAGE OverloadedRecordDot #-}

module AuthTest where

import Control.Monad.Except (ExceptT, MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.State (MonadState (get), StateT, modify')
import Crypto.KDF.PBKDF2 (
  Parameters (..),
  fastPBKDF2_SHA512,
 )
import Crypto.Random.Entropy (getEntropy)
import Data.Foldable (find)
import Data.Time (getCurrentTime)
import DataAccess.Data (Login, User (..))
import Logic.Auth (AuthError, Handle (..))

type TestM = ExceptT AuthError (StateT [User] IO)

getUser :: Login -> TestM (Maybe User)
getUser login =
  find
    (\u -> u.userLogin == login)
    <$> lift get

addUser :: User -> TestM (Maybe User)
addUser user = do
  mu <- getUser user.userLogin
  case mu of
    Nothing -> do
      modify' (user :)
      pure $ Just user
    Just _ -> pure Nothing

dummyHandle :: Handle TestM
dummyHandle =
  Handle
    { _generateSalt = liftIO $ getEntropy 32
    , _currentTime = liftIO getCurrentTime
    , _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = 32
            , outputLength = 32
            }
    , _addToDb = addUser
    , _getUser = getUser
    , _setCookie = undefined -- const (pure $ Just _a)
    , _throw = undefined
    , _logger = undefined
    }