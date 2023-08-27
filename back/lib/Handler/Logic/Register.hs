{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Logic.Register where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Entities.User (Login)
import Dto.Auth (Credential (..))
import Handler.Types.Register (CreateUserData (..), Handle (..), RegistrationError (..))

parseRequest :: (Monad m) => Handle m -> Credential -> m (Login, Text)
parseRequest Handle{..} credential = do
  when (T.length (login credential) < 5) (_throw InvalidRequest)
  when (T.length (password credential) < 5) (_throw InvalidRequest)
  pure (login credential, password credential)

register :: (Monad m) => Handle m -> Credential -> m ()
register h@Handle{..} credential = do
  (login, password) <- parseRequest h credential
  createdAt <- _currentTime
  salt <- _generateSalt
  let hashedPassword = _hashPassword (encodeUtf8 password) salt
  _ <-
    _addUser
      CreateUserData
        { _createdAt = createdAt
        , _login = login
        , _hashedPassword = hashedPassword
        , _salt = salt
        }
  pure ()
