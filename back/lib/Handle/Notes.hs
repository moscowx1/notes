{-# LANGUAGE RankNTypes #-}

module Handle.Notes where

import Api (Payload)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (liftIO), withExceptT)
import DataAccess.Data (Note)
import qualified DataAccess.Notes as DataAccess
import Dto.Note (CreateNoteReq, GetNoteReq)
import qualified Handle.Logger as Logger
import Logic.Notes (Error (..), Handle (..))
import qualified Logic.Notes as LN
import Servant (Handler (Handler), ServerError (errReasonPhrase), err400, err404)
import Types (SqlRuner)

handle ::
  SqlRuner ->
  Logger.Handle (ExceptT Error IO) ->
  Handle (ExceptT Error IO)
handle runer l =
  Handle
    { _createNote = liftIO . runer . DataAccess.createNote
    , _getUserId = liftIO . runer . DataAccess.getUserId
    , _getNote = liftIO . runer . DataAccess.getNote
    , _logger = l
    , _throw = throwError
    }

mapper :: Error -> ServerError
mapper InvalidContent = err400{errReasonPhrase = "Empty content"}
mapper NoteNotFound = err404{errReasonPhrase = "Note not found"}
mapper UserNotFound = err400{errReasonPhrase = "User not found"}

getNote ::
  SqlRuner ->
  Logger.Handle (ExceptT Error IO) ->
  GetNoteReq ->
  Handler Note
getNote runer logger = Handler . get'
 where
  h = handle runer logger
  get' = withExceptT mapper . LN.getNote h

createNote ::
  SqlRuner ->
  Payload ->
  Logger.Handle (ExceptT Error IO) ->
  CreateNoteReq ->
  Handler ()
createNote runer payload logger = Handler . create'
 where
  h = handle runer logger
  create' = withExceptT mapper . LN.create h payload
