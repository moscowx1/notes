module Types (
  Password,
  Salt,
  Login,
  HashedPassword,
  SqlBackT,
) where

import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Esqueleto.Experimental (SqlBackend)

type Password = ByteString
type Salt = ByteString
type Login = Text
type HashedPassword = ByteString

type SqlBackT m = ReaderT SqlBackend (NoLoggingT m)
