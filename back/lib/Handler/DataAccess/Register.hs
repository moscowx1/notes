{-# LANGUAGE LambdaCase #-}

module Handler.DataAccess.Register where

import Data.Time (UTCTime)
import Database.Beam (SqlValable (val_), default_, insertExpressions)
import Database.Beam.Backend.SQL.BeamExtensions (
  BeamHasInsertOnConflict (
    conflictingFields,
    insertOnConflict,
    onConflictDoNothing
  ),
  runInsertReturningList,
 )
import Database.Beam.Postgres (Pg)
import Database.Entities.User (User, UserT (..))
import Database.NoteDb (NotesDb (_users), notesDb)
import Handler.Logic.Register (HashedPassword, Login, Salt)

addUser :: UTCTime -> Login -> HashedPassword -> Salt -> Pg (Maybe User)
addUser createdAt login password salt =
  runInsertReturningList
    ( insertOnConflict
        (_users notesDb)
        ( insertExpressions
            [ User
                { _userId = default_
                , _userCreatedAt = val_ createdAt
                , _userLogin = val_ login
                , _userPassword = val_ password
                , _userSalt = val_ salt
                }
            ]
        )
        (conflictingFields _userLogin)
        onConflictDoNothing
    )
    >>= \case
      [u] -> pure $ Just u
      [] -> pure Nothing
      _ -> error "impossible"
