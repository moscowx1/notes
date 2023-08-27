module Handler.Handler.Register where

import qualified Config.Auth as CA
import qualified Config.Config as CG
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA512)
import Crypto.Random.Entropy (getEntropy)
import Data.Time (getCurrentTime)
import Database.Beam.Postgres (Connection)
import Handler.DataAccess.Register (addUser)
import Handler.Types.Register (Handle (..), RegistrationError)

register ::
  Connection ->
  CG.Config ->
  Handle (ExceptT RegistrationError IO)
register connection config =
  Handle
    { _addUser = liftIO . addUser connection
    , _throw = throwError
    , _connection = connection
    , _currentTime = liftIO getCurrentTime
    , _hashPassword =
        fastPBKDF2_SHA512 $
          Parameters
            { iterCounts = CA._generatingIterCount $ CG._auth config
            , outputLength = CA._hashedPasswordLength $ CG._auth config
            }
    , _generateSalt = liftIO $ getEntropy $ CA._saltLength $ CG._auth config
    }
