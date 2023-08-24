{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.NoteDb where

import Database.Beam (Database, DatabaseSettings, Generic, TableEntity, dbModification, defaultDbSettings, fieldNamed, modifyTableFields, setEntityName, tableModification, withDbModification)
import Database.Entities.Note (NoteT (..))
import Database.Entities.Tag (TagT (..))
import Database.Entities.User (PrimaryKey (UserId), UserT (..))

data NotesDb f = NotesDb
  { _users :: f (TableEntity UserT)
  , _notes :: f (TableEntity NoteT)
  , _tags :: f (TableEntity TagT)
  }
  deriving (Generic, Database be)

notesDb :: DatabaseSettings be NotesDb
notesDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _users =
          setEntityName "user"
            <> modifyTableFields
              tableModification
                { _userId = fieldNamed "id"
                , _userLogin = fieldNamed "login"
                , _userSalt = fieldNamed "salt"
                , _userPassword = fieldNamed "password"
                , _userCreatedAt = fieldNamed "created_at"
                }
      , _notes =
          setEntityName "note"
            <> modifyTableFields
              tableModification
                { _noteId = fieldNamed "id"
                , _noteContent = fieldNamed "content"
                , _noteAuthor = UserId (fieldNamed "author")
                , _noteTitle = fieldNamed "title"
                }
      , _tags =
          setEntityName "tag"
            <> modifyTableFields
              tableModification
                { _tagId = fieldNamed "id"
                , _tagValue = fieldNamed "value"
                , _tagAuthor = UserId (fieldNamed "author")
                , _tagCreatedAt = fieldNamed "created_at"
                }
      }
