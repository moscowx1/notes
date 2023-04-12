{-# LANGUAGE RankNTypes #-}

module Types (
  Password,
  Salt,
  Login,
  HashedPassword,
  SqlBack,
  SqlBackT,
  SqlRuner
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

type SqlRuner = forall a. SqlBack a -> IO a
type SqlBack = ReaderT SqlBackend (NoLoggingT IO)
type SqlBackT m = ReaderT SqlBackend (NoLoggingT m)
