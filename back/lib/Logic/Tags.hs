{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import DataAccess.Data (Tag (..), UserId)
import Database.Esqueleto (Value (Value))
import Dto.Tag (CreateTagReq (CreateTagReq, value), SearchTagsReq (..))
import Handle.Logger (_logDebug, _logError, _logInfo)
import qualified Handle.Logger as Logger

data TagError
  = TagAlreadyExists
  | TooBigLimitValue
  | InvalidValue

data Handle m = Handle
  { _addToDb :: Tag -> m (Maybe Tag)
  , _search :: Text -> Int64 -> UserId -> m [Tag]
  , _currentTime :: m UTCTime
  , _throw :: forall a. TagError -> m a
  , _logger :: Logger.Handle m
  }

addTag ::
  (Monad m) =>
  Handle m ->
  Text ->
  UserId ->
  m Tag
addTag Handle{..} tagValue tagAuthor = do
  _logInfo _logger "adding tag to database"
  tagCreatedAt <- _currentTime
  let tag = Tag{..}
  _addToDb tag >>= \case
    Nothing -> do
      _logError _logger "tag already exists"
      _throw TagAlreadyExists
    Just x -> pure x

validateTagValue ::
  (Monad m) =>
  Handle m ->
  Text ->
  m Text
validateTagValue Handle{..} value =
  if T.length value < 2
    then do
      _logError _logger "invalid tag"
      _throw InvalidValue
    else pure value

data ValidSearchTagReq = ValidSearchTagReq
  { _limit :: Int
  , _searchPhrase :: Text
  }

validateSearch ::
  (Monad m) =>
  Handle m ->
  SearchTagsReq ->
  m ValidSearchTagReq
validateSearch Handle{..} SearchTagsReq{..} = do
  when (_limit > _)

createTag ::
  (Monad m) =>
  Handle m ->
  CreateTagReq ->
  UserId ->
  m Tag
createTag h@Handle{..} CreateTagReq{..} userId = do
  _logDebug _logger "validating tag"
  when (T.length value < 1) (_throw InvalidValue)
  addTag h value userId

searchTags ::
  (Monad m) =>
  Handle m ->
  SearchTagsReq ->
  UserId ->
  m Tag
searchTags h SearchTagsReq{..} userId = undefined
