{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module DataAccess.Auth (addUser, userByLogin) where

import Control.Monad.Reader (MonadIO)
import DataAccess.Data (EntityField (UserLogin), Login, User)
import Database.Esqueleto.Experimental (SqlPersistT, (==.), (^.))
import qualified Database.Esqueleto.Experimental as E

addUser ::
  MonadIO m =>
  User ->
  SqlPersistT m (Maybe User)
addUser user = do
  res <- E.insertUniqueEntity user
  pure $ E.entityVal <$> res

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
