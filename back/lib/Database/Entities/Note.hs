{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Entities.Note where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Beamable, C, Generic, Identity, Table (PrimaryKey, primaryKey))
import Database.Entities.User (UserT)

data NoteT f = Note
  { _noteId :: C f Int32
  , _noteContent :: C f Text
  , _noteAuthor :: PrimaryKey UserT f
  , _noteTitle :: C f Text
  }
  deriving (Generic, Beamable)

type Note = NoteT Identity

deriving instance Show Note
deriving instance Eq Note

instance Table NoteT where
  data PrimaryKey NoteT f = NoteId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey :: NoteT column -> PrimaryKey NoteT column
  primaryKey = NoteId . _noteId

type NoneId = PrimaryKey NoteT Identity
