{-# LANGUAGE TypeApplications #-}

module DataAccess.Notes where

import Control.Monad.IO.Class (MonadIO)
import DataAccess.Data (EntityField (UserLogin), Login, Note, User)
import Database.Esqueleto.Experimental (SqlPersistT, (==.), (^.))
import qualified Database.Esqueleto.Experimental as E

createNote ::
  (MonadIO m) =>
  Note ->
  SqlPersistT m Note
createNote = (E.entityVal <$>) . E.insertEntity

getNote ::
  (MonadIO m) =>
  E.Key Note ->
  SqlPersistT m (Maybe Note)
getNote = E.get

getUserId ::
  (MonadIO m) =>
  Login ->
  SqlPersistT m (Maybe (E.Key User))
getUserId l = do
  mu <- E.selectOne $ do
    u <- E.from $ E.table @User
    E.where_ (u ^. UserLogin ==. E.val l)
    pure u
  pure (E.entityKey <$> mu)
