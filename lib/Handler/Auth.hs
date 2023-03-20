{-# LANGUAGE RecordWildCards #-}

module Handler.Auth (register) where

import Logic.Auth (Handle(..))
import qualified Logic.Auth as LA
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA512)
import Config.Auth (Config (..))
import DataAccess.Auth (addUser, userByLogin)
import Control.Monad.Reader (MonadIO (liftIO))
import DataAccess.Data (User)
import Dto (UnvalidatedCredential)
import Types (SqlBackT)

handle :: (MonadIO m) => Config -> Handle (SqlBackT m)
handle Config {..} =
  Handle
  { _generateSalt = liftIO $ getEntropy _saltLength
  , _currentTime = liftIO getCurrentTime
  , _hashPassword = fastPBKDF2_SHA512 $ Parameters
      { iterCounts = _generatingIterCount
      , outputLength = _hashedPasswordLength
      }
  , _addToDb = addUser
  , _getUser = userByLogin
  }

register
  :: MonadIO m
  => Config
  -> UnvalidatedCredential
  -> SqlBackT m (Maybe User)
register = LA.register . handle
