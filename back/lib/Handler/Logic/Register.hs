{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Logic.Register where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Database.Beam.Postgres (Connection)
import Database.Entities.User (User)
import Dto.Auth (Credential (..))

data ValidCredential = ValidCredential
  { _login :: Text
  , _password :: Text
  }

data RegistrationError
  = InvalidRequest

type Salt = ByteString
type HashedPassword = ByteString
type Login = Text

data Handle m = Handle
  { _addUser :: Connection -> User -> m ()
  , _connection :: Connection
  , _throw :: forall a. RegistrationError -> m a
  , _currentTime :: m UTCTime
  , _hashPassword :: ByteString -> Salt -> HashedPassword
  , _generateSalt :: m Salt
  }

parseRequest :: (Monad m) => Handle m -> Credential -> m ValidCredential
parseRequest Handle{..} credential = do
  when (T.length (login credential) < 5) (_throw InvalidRequest)
  when (T.length (password credential) < 5) (_throw InvalidRequest)
  pure $
    ValidCredential
      { _login = login credential
      , _password = password credential
      }

hashPassword ::
  (Monad m) =>
  Handle m ->
  Text ->
  m HashedPassword
hashPassword Handle{..} password = do
  salt <- _generateSalt
  let bsPassword = encodeUtf8 password
  pure $ _hashPassword bsPassword salt

register :: (Monad m) => Handle m -> Credential -> m ()
register h@Handle{..} credential = do
  validCred <- parseRequest h credential
  _userCreatedAt <- _currentTime
  _userPassword <- hashPassword h (_password validCred)
  _addUser _connection undefined
