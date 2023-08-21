{-# LANGUAGE RankNTypes #-}

module Handle.Tags where

import Api (Payload (login))
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (liftIO), withExceptT)
import Data.Int (Int64)
import Data.Time (getCurrentTime)
import DataAccess.Data (Tag)
import qualified DataAccess.Tags as DataAccess
import Dto.Tag (CreateTagReq, SearchTagsReq)
import qualified Handle.Logger as Logger
import Logic.Tags (Handle (..), TagError (..))
import qualified Logic.Tags as L
import Servant (Handler (Handler), ServerError (errReasonPhrase), err400, err409)
import Types (SqlRuner)

handle ::
  SqlRuner ->
  Int64 ->
  Logger.Handle (ExceptT TagError IO) ->
  Handle (ExceptT TagError IO)
handle runner limit logger =
  Handle
    { _addToDb = liftIO . runner . DataAccess.createTag
    , _search = liftIO . runner . DataAccess.searchTags
    , _currentTime = liftIO getCurrentTime
    , _throw = throwError
    , _logger = logger
    , _searchLimit = limit
    }

mapper :: TagError -> ServerError
mapper TagAlreadyExists = err409{errReasonPhrase = "Tag already exists"}
mapper InvalidValue = err400{errReasonPhrase = "invalid note content"}
mapper InvalidLimit = err400{errReasonPhrase = "invalid limit value"}

createTag ::
  SqlRuner ->
  Payload ->
  Int64 ->
  Logger.Handle (ExceptT TagError IO) ->
  CreateTagReq ->
  Handler Tag
createTag runner payload limit logger req = Handler create
 where
  h = handle runner limit logger
  -- TODO: set id
  create = withExceptT mapper $ L.createTag h req (login payload)

searchTags ::
  SqlRuner ->
  Payload ->
  Int64 ->
  Logger.Handle (ExceptT TagError IO) ->
  SearchTagsReq ->
  Handler [Tag]
searchTags runner payload limit logger req = Handler search
 where
  h = handle runner limit logger
  search = withExceptT mapper $ L.searchTags h req (login payload)
