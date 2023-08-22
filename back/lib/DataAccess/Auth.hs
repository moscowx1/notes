{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module DataAccess.Auth (addUser, userByLogin) where

import Control.Monad.Reader (MonadIO)
import DataAccess.Data (EntityField (UserLogin), Login, User)
import Database.Esqueleto.Experimental (SqlPersistT, (==.), (^.))
import qualified Database.Esqueleto.Experimental as E

addUser ::
  (MonadIO m) =>
  User ->
  SqlPersistT m (Maybe (E.Entity User))
addUser user = do
  E.insertUniqueEntity user

userByLogin ::
  (MonadIO m) =>
  Login ->
  SqlPersistT m (Maybe (E.Entity User))
userByLogin login = do
  E.selectOne $ do
    u <- E.from $ E.table @User
    E.where_ (u ^. UserLogin ==. E.val login)
    pure u
