{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Scheme 
  ( UserT(..)
  , User
  , UserId
  , NoteT(..)
  , Note
  , NoteId
  , NoteDb
  , getConnection)
    where

import Data.Functor.Identity (Identity)
import Database.Beam.Migrate (CheckedDatabaseSettings)
import Database.Beam.Postgres (Connection, Postgres)
import Database.Migration (migrateDb)
import Database.Migrations.V0001
  ( NoteT(..)
  , UserT(..)
  , PrimaryKey(UserId, NoteId)
  , NoteDb)

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

type UserId = PrimaryKey UserT Identity
deriving instance Show UserId

type Note = NoteT Identity
deriving instance Show Note

type NoteId = PrimaryKey NoteT Identity
deriving instance Show NoteId

getConnection :: Connection
  -> IO (Maybe (CheckedDatabaseSettings Postgres NoteDb))
getConnection = migrateDb

