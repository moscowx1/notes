module DataAccess.Notes where

import Control.Monad.IO.Class (MonadIO)
import DataAccess.Data (Note)
import Database.Esqueleto.Experimental (SqlPersistT)
import qualified Database.Esqueleto.Experimental as E

addNote ::
  (MonadIO m) =>
  Note ->
  SqlPersistT m Note
addNote = (E.entityVal <$>) . E.insertEntity

getNote ::
  (MonadIO m) =>
  E.Key Note ->
  SqlPersistT m (Maybe Note)
getNote = E.get
