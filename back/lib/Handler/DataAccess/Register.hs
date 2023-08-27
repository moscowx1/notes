{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.DataAccess.Register where

import Database.Beam (SqlValable (val_), default_, insertExpressions)
import Database.Beam.Backend.SQL.BeamExtensions (
  BeamHasInsertOnConflict (
    conflictingFields,
    insertOnConflict,
    onConflictDoNothing
  ),
  runInsertReturningList,
 )
import Database.Beam.Postgres (Connection, runBeamPostgres)
import Database.Entities.User (User, UserT (..))
import Database.NoteDb (NotesDb (_users), notesDb)
import Handler.Types.Register (CreateUserData (..))

addUser :: Connection -> CreateUserData -> IO (Maybe User)
addUser connection CreateUserData{..} =
  runBeamPostgres connection $
    runInsertReturningList
      ( insertOnConflict
          (_users notesDb)
          ( insertExpressions
              [ User
                  { _userId = default_
                  , _userCreatedAt = val_ _createdAt
                  , _userLogin = val_ _login
                  , _userPassword = val_ _hashedPassword
                  , _userSalt = val_ _salt
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
