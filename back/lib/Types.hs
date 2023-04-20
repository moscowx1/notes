{-# LANGUAGE RankNTypes #-}

module Types (
  SqlBack,
  SqlBackT,
  SqlRuner,
) where

import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Reader (ReaderT)
import Database.Esqueleto.Experimental (SqlBackend)

type SqlRuner = forall a. SqlBack a -> IO a
type SqlBack = ReaderT SqlBackend (NoLoggingT IO)
type SqlBackT m = ReaderT SqlBackend (NoLoggingT m)
