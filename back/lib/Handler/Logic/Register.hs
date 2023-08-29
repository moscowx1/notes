{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Logic.Register where

import Api (Payload (..))
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.Entities.User (Login, User, UserT (_userId, _userLogin))
import qualified Dto.Auth as Dto
import Handler.Types.Register (CreateUserData (..), Handle (..), RegistrationError (..))

parseRequest :: (Monad m) => Handle m r -> Dto.Credential -> m (Login, Text)
parseRequest Handle{..} credential = do
  when (T.length (Dto.login credential) < 5) (_throw InvalidRequest)
  when (T.length (Dto.password credential) < 5) (_throw InvalidRequest)
  pure (Dto.login credential, Dto.password credential)

addUser ::
  (Monad m) =>
  Handle m r ->
  CreateUserData ->
  m User
addUser h createData =
  _addUser h createData >>= \case
    Nothing -> _throw h LoginAlreadyTaken
    Just x -> pure x

register :: (Monad m) => Handle m r -> Dto.Credential -> m r
register h@Handle{..} credential = do
  (login, password) <- parseRequest h credential
  createdAt <- _currentTime
  salt <- _generateSalt
  let hashedPassword = _hashPassword (encodeUtf8 password) salt
      createData =
        CreateUserData
          { _createdAt = createdAt
          , _login = login
          , _hashedPassword = hashedPassword
          , _salt = salt
          }
  user <- addUser h createData
  let payload = Payload{login = _userLogin user, userId = _userId user}
  _setCookie payload >>= \case
    Nothing -> _throw ErrorSettingCookie
    Just x -> pure x
