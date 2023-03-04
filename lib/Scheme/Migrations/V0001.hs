module Scheme.Migrations.V0001 where

import Database.Beam.Backend 
  ( BeamSqlBackend
  , IsSql92DataTypeSyntax (timestampType)
  )
import Database.Beam (DataType(DataType), int, nationalVarchar)
import Data.Time (UTCTime)
import Database.Beam.Migrate (Migration, CheckedDatabaseSettings, createTable, notNull, unique, field)
import Database.Beam.Postgres (Postgres (Postgres))
import Scheme.NoteDb (NoteDb (NoteDb))
import Scheme.User (UserT(..), PrimaryKey(UserId))
import Scheme.Note (NoteT(..))


utctime :: BeamSqlBackend be => DataType be UTCTime
utctime = DataType (timestampType Nothing True)

initialSetup :: Migration Postgres
  (CheckedDatabaseSettings Postgres NoteDb)
initialSetup = NoteDb
  <$> (createTable "users" $ User
        { _userId = field "id"
            int notNull unique
        , _userNick = field "nick"
            (nationalVarchar (Just 20)) notNull unique
        })
  <*> (createTable "notes" $ Note
        { _noteId = field "id"
            int notNull unique
        , _noteContent = field "content"
            (nationalVarchar (Just 5000)) notNull
        , _noteAuthorId = UserId $
            field "author_id" int notNull
        })

