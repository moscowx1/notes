{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Tags (Handle (..), TagError (..), createTag, searchTags) where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import DataAccess.Data (Tag (..), UserId)
import DataAccess.Tags (ValidSearchData (..))
import Dto.Tag (CreateTagReq (..), SearchTagsReq (..))
import Handle.Logger (_logDebug, _logError, _logInfo)
import qualified Handle.Logger as Logger

data TagError
  = TagAlreadyExists
  | InvalidValue
  | InvalidLimit

data Handle m = Handle
  { _addToDb :: Tag -> m (Maybe Tag)
  , _search :: ValidSearchData -> m [Tag]
  , _currentTime :: m UTCTime
  , _throw :: forall a. TagError -> m a
  , _logger :: Logger.Handle m
  , _searchLimit :: Int64
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

validateSearchData ::
  (Monad m) =>
  Handle m ->
  SearchTagsReq ->
  Text ->
  m ValidSearchData
validateSearchData h@Handle{..} SearchTagsReq{..} _userLogin = do
  when (_limit > _searchLimit) (_logError _logger "invalid limit" >> _throw InvalidLimit)
  validateTagValue h _searchPhrase >> pure ValidSearchData{..}

searchTags' ::
  (Monad m) =>
  Handle m ->
  ValidSearchData ->
  m [Tag]
searchTags' Handle{..} searchData = do
  _logInfo _logger "searching tags"
  _search searchData

createTag ::
  (Monad m) =>
  Handle m ->
  CreateTagReq ->
  UserId ->
  m Tag
createTag h@Handle{..} CreateTagReq{..} userId = do
  _logDebug _logger "validating request"
  validateTagValue h _value >> addTag h _value userId

searchTags ::
  (Monad m) =>
  Handle m ->
  SearchTagsReq ->
  Text ->
  m [Tag]
searchTags h@Handle{..} req userLogin = do
  _logDebug _logger "validating request"
  validData <- validateSearchData h req userLogin
  searchTags' h validData
