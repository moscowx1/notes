{-# LANGUAGE RankNTypes #-}

module Handle.Notes where

import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (liftIO))
import DataAccess.Notes (createNote, getNote, getUserId)
import qualified Handle.Logger as Logger
import Logic.Notes (Error (..), GetNoteReq, Handle (..))
import Servant (Handler, ServerError (errReasonPhrase), err400, err404)
import Types (SqlRuner)

handle ::
  SqlRuner ->
  Logger.Handle (ExceptT Error IO) ->
  Handle (ExceptT Error IO)
handle runer l =
  Handle
    { _createNote = liftIO . runer . createNote
    , _getUserId = liftIO . runer . getUserId
    , _getNote = liftIO . runer . getNote
    , _logger = l
    , _throw = throwError
    }

mapper :: Error -> ServerError
mapper InvalidContent = err400{errReasonPhrase = "Empty content"}
mapper NoteNotFound = err404{errReasonPhrase = "Note not found"}
mapper UserNotFound = err400{errReasonPhrase = "User not found"}

get ::
  SqlRuner ->
  Logger.Handle (ExceptT Error IO) ->
  GetNoteReq ->
  Handler a
get runer logger req = undefined

{-
register ::
  SqlRuner ->
  Logger.Handle (ExceptT AuthError IO) ->
  Config ->
  JwtHeaderSetter IO ->
  RegisterReq ->
  Handler JwtHeader
register r l c jwtSet = Handler . reg'
 where
  h = handle r l c jwtSet
  reg' = withExceptT mapper . LA.register h
-}
