{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Auth (register, signIn) where

import Config.Auth (Config (..))
import Control.Monad.Except (ExceptT, MonadIO (liftIO))
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import DataAccess.Auth (addUser, userByLogin)
import DataAccess.Data (User)
import Dto.Auth (RegisterReq)
import Logic.Auth (Handle (..))
import qualified Logic.Auth as LA
import Types (SqlBackT)

handle :: (MonadIO m) => Config -> Handle (SqlBackT m)
handle Config{..} =
  Handle
    { _generateSalt = liftIO $ getEntropy _saltLength
    , _currentTime = liftIO getCurrentTime
    , _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = _generatingIterCount
            , outputLength = _hashedPasswordLength
            }
    , _addToDb = addUser
    , _getUser = userByLogin
    }

register ::
  MonadIO m =>
  Config ->
  RegisterReq ->
  ExceptT String (SqlBackT m) User
register = LA.register . handle

signIn ::
  MonadIO m =>
  Config ->
  RegisterReq ->
  ExceptT String (SqlBackT m) User
signIn = LA.signIn . handle
