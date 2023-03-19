{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}

module DataAccess.Auth (addUser) where

import qualified Database.Esqueleto.Experimental as E
import DataAccess.Data (User)
import Control.Monad.Reader (MonadIO)
import Database.Esqueleto.Experimental (SqlPersistT)
import Types (Login)

addUser
  :: MonadIO m
  => User
  -> SqlPersistT m (Maybe User)
addUser user = do
  res <- E.insertUniqueEntity user
  pure $ case res of
    Nothing -> Nothing
    Just x -> Just (E.entityVal x)

{-
userByLogin
  :: MonadIO m
  => Login
  -> SqlPersistT m (Maybe User)
userByLogin login = do
  mUser <- E.selectOne $ do
  pure Nothing
-}