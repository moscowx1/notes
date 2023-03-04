module Scheme.Migrations.V0001 where

import Data.Time (UTCTime)

import Database.Beam (DataType(DataType), int, nationalVarchar)
import Database.Beam.Backend ( BeamSqlBackend, timestampType)
import qualified Database.Beam.Migrate as BM
import Database.Beam.Postgres (Postgres (Postgres))

import Scheme.Note (NoteT(..))
import Scheme.NoteDb (NoteDb (NoteDb))
import Scheme.User (UserT(..), PrimaryKey(UserId))

utctime :: BeamSqlBackend be => DataType be UTCTime
utctime = DataType (timestampType Nothing True)

initialSetup :: BM.Migration Postgres
  (BM.CheckedDatabaseSettings Postgres NoteDb)
initialSetup = NoteDb
  <$> (BM.createTable "users" $ User
        { _userId = BM.field "id"
            int BM.notNull BM.unique
        , _userNick = BM.field "nick"
            (nationalVarchar (Just 20)) BM.notNull BM.unique
        })
  <*> (BM.createTable "notes" $ Note
        { _noteId = BM.field "id"
            int BM.notNull BM.unique
        , _noteContent = BM.field "content"
            (nationalVarchar (Just 5000)) BM.notNull
        , _noteAuthorId = UserId $
            BM.field "author_id" int BM.notNull
        })

