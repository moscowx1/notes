{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

module DataAccess.Auth2 (addUser, userByLogin) where

import Control.Monad.Reader (MonadIO (liftIO), MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import DataAccess.Data (EntityField (UserLogin), User)
import Database.Esqueleto.Experimental (SqlPersistT, (==.), (^.))
import qualified Database.Esqueleto.Experimental as E
import Types (Login)

addUser ::
  MonadIO m =>
  User ->
  MaybeT (SqlPersistT m) User
addUser user = do
  z <- lift $ E.insertUniqueEntity user
  case z of
    Nothing -> MaybeT $ pure Nothing
    Just x -> pure $ E.entityVal x

userByLogin ::
  MonadIO m =>
  Login ->
  MaybeT (SqlPersistT m) User
userByLogin login = do
  mu <- lift $ E.selectOne $ do
    u <- E.from $ E.table @User
    E.where_ (u ^. UserLogin ==. E.val login)
    pure u
  MaybeT $ pure (E.entityVal <$> mu)
