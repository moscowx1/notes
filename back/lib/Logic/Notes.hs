{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Logic.Notes where

import Api (Payload (..))
import Control.Monad (void)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import DataAccess.Data (Login, Note (..), NoteId, UserId)
import Database.Esqueleto.Experimental (toSqlKey)
import Handle.Logger (_logDebug, _logError, _logInfo)
import qualified Handle.Logger as Logger

type CreateNoteReq = Text

newtype GetNoteReq = GetNoteReq {_id :: Int64}

data Error
  = InvalidContent
  | NoteNotFound
  | UserNotFound

data Handle m = Handle
  { _createNote :: Note -> m Bool
  , _getUserId :: Login -> m (Maybe UserId)
  , _getNote :: NoteId -> m (Maybe Note)
  , _logger :: Logger.Handle m
  , _throw :: forall a. Error -> m a
  }

type ValidCreateReq = Text

validCreateReq ::
  (Monad m) =>
  Handle m ->
  CreateNoteReq ->
  m ValidCreateReq
validCreateReq Handle{..} req = do
  _logDebug _logger "validating request"
  if T.length req == 0
    then do
      _logError _logger "validating request"
      _throw InvalidContent
    else pure req

createNote ::
  (Monad m) =>
  Handle m ->
  Note ->
  m ()
createNote Handle{..} note = do
  _logInfo _logger "creating note"
  void $ _createNote note

getUser ::
  (Monad m) =>
  Handle m ->
  Login ->
  m UserId
getUser Handle{..} login = do
  _logInfo _logger "getting user by id"
  _getUserId login >>= \case
    Nothing -> do
      _logError _logger "user not found"
      _throw UserNotFound
    Just x -> pure x

create ::
  (Monad m) =>
  Handle m ->
  Payload ->
  CreateNoteReq ->
  m ()
create h p req = do
  noteContent <- validCreateReq h req
  noteAuthor <- getUser h (login p)
  let note = Note{..}
  createNote h note

getNote ::
  (Monad m) =>
  Handle m ->
  NoteId ->
  m Note
getNote Handle{..} _id = do
  _logInfo _logger "getting note from db"
  _getNote _id >>= \case
    Nothing -> do
      _logError _logger "note not found"
      _throw NoteNotFound
    Just x -> pure x

get ::
  (Monad m) =>
  Handle m ->
  GetNoteReq ->
  m Note
get h req = do
  getNote h $ toSqlKey (_id req)
