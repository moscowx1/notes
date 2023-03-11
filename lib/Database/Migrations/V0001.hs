{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migrations.V0001
  ( NoteT(..)
  , UserT(..)
  , NoteDb(..)
  , B.PrimaryKey(NoteId, UserId)
  , initialSetupStep
  ) 
    where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate as BM
import Database.Beam.Postgres (Postgres)

data NoteT f
  = Note
  { _noteId :: B.Columnar f Int32
  , _noteContent :: B.Columnar f Text
  , _noteAuthorId :: B.PrimaryKey UserT f
  } deriving (B.Beamable, B.Generic)

instance B.Table NoteT where
  data PrimaryKey NoteT f = NoteId (B.Columnar f Int32)
    deriving (B.Beamable, B.Generic)
  primaryKey = NoteId . _noteId

data UserT f
  = User
    { _userId :: B.Columnar f Int32
    , _userNick :: B.Columnar f Text
    } deriving (B.Generic, B.Beamable)

instance B.Table UserT where
  data PrimaryKey UserT f = UserId (B.Columnar f Int32)
    deriving (B.Generic, B.Beamable)
  primaryKey = UserId . _userId

data NoteDb f
  = NoteDb
  { dbUsers :: f (B.TableEntity UserT)
  , dbNotes :: f (B.TableEntity NoteT)
  } deriving (B.Database be, B.Generic)

initialSetup :: BM.Migration Postgres
  (BM.CheckedDatabaseSettings Postgres NoteDb)
initialSetup = NoteDb
  <$> (BM.createTable "users" $ User
        { _userId = BM.field "id"
            B.int BM.notNull BM.unique
        , _userNick = BM.field "nick"
            (B.nationalVarchar (Just 20)) BM.notNull BM.unique
        })
  <*> (BM.createTable "notes" $ Note
        { _noteId = BM.field "id"
            B.int BM.notNull BM.unique
        , _noteContent = BM.field "content"
            (B.nationalVarchar (Just 5000)) BM.notNull
        , _noteAuthorId = UserId $
            BM.field "author_id" B.int BM.notNull
        })

initialSetupStep :: BM.MigrationSteps Postgres
  ()
  (BM.CheckedDatabaseSettings Postgres NoteDb)
initialSetupStep = BM.migrationStep
  "initial_setup"
  (const initialSetup)
