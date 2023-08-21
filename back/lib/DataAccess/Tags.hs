{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module DataAccess.Tags where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Text (Text)
import DataAccess.Data (EntityField (TagAuthor, TagValue, UserId, UserLogin), Tag, User (User))
import Database.Esqueleto.Experimental ((&&.), (:&) ((:&)), (==.), (^.))
import qualified Database.Esqueleto.Experimental as E

createTag ::
  (MonadIO m) =>
  Tag ->
  E.SqlPersistT m (Maybe Tag)
createTag tag = do
  res <- E.insertUniqueEntity tag
  pure $ E.entityVal <$> res

findUserByLogin ::
  (MonadIO m) =>
  Text ->
  E.SqlPersistT m (Maybe User)
findUserByLogin login = E.selectOne (do
  u <- E.from $ E.table @User
  E.where_ (u ^. UserLogin ==. E.val login)
  pure u) <&> (E.entityVal <$>)

data ValidSearchData = ValidSearchData
  { _limit :: Int64
  , _searchPhrase :: Text
  , _userLogin :: Text
  }

searchTags ::
  (MonadIO m) =>
  ValidSearchData ->
  E.SqlPersistT m [Tag]
searchTags ValidSearchData{..} =
  ( E.select $ do
      (tag :& user) <-
        E.from $
          E.table @Tag
            `E.innerJoin` E.table @User
              `E.on` ( \(tag :& user) ->
                        tag ^. TagAuthor ==. user ^. UserId
                     )
      E.where_
        ( user ^. UserLogin ==. E.val _userLogin
            &&. E.ilike (tag ^. TagValue) (E.val _searchPhrase)
        )
      E.limit _limit
      pure (tag, user)
  )
    <&> (E.entityVal . fst <$>)
