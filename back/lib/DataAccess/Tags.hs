{-# LANGUAGE TypeApplications #-}

import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import Data.Text (Text)
import DataAccess.Data (EntityField (TagAuthor, TagValue), Tag, UserId)
import Database.Esqueleto.Experimental ((&&.), (==.), (^.))
import qualified Database.Esqueleto.Experimental as E

createTag ::
  (MonadIO m) =>
  Tag ->
  E.SqlPersistT m (Maybe Tag)
createTag tag = do
  res <- E.insertUniqueEntity tag
  pure $ E.entityVal <$> res

searchTags ::
  (MonadIO m) =>
  Text ->
  Int64 ->
  UserId ->
  E.SqlPersistT m [Tag]
searchTags search limit id' = do
  tags <- E.select $ do
    tag <- E.from $ E.table @Tag
    E.where_ (E.ilike (tag ^. TagValue) (E.val search) &&. tag ^. TagAuthor ==. E.val id')
    E.limit limit
    pure tag
  pure $ E.entityVal <$> tags
