{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module DataAccess.Auth (addUser, userByLogin) where

import Control.Monad.Reader (MonadIO)
import DataAccess.Data (EntityField (UserLogin), User)
import Database.Esqueleto.Experimental (SqlPersistT, (==.), (^.))
import qualified Database.Esqueleto.Experimental as E
import Types (Login)

addUser ::
  MonadIO m =>
  User ->
  SqlPersistT m (Maybe User)
addUser user = do
  res <- E.insertUniqueEntity user
  pure $ case res of
    Nothing -> Nothing
    Just x -> Just (E.entityVal x)

userByLogin ::
  MonadIO m =>
  Login ->
  SqlPersistT m (Maybe User)
userByLogin login = do
  mu <- E.selectOne $ do
    u <- E.from $ E.table @User
    E.where_ (u ^. UserLogin ==. E.val login)
    pure u
  pure (E.entityVal <$> mu)
