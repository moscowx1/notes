{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Scheme.Note (NoteT(..), PrimaryKey(NoteId)) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam 
  ( Beamable
  , Columnar
  , Generic
  , PrimaryKey
  , primaryKey
  , Table
  )
import Scheme.User (UserT)

data NoteT f
  = Note
  { _noteId :: Columnar f Int32
  , _noteContent :: Columnar f Text
  , _noteAuthorId :: PrimaryKey UserT f
  } deriving (Beamable, Generic)

instance Table NoteT where
  data PrimaryKey NoteT f = NoteId (Columnar f Int32)
    deriving (Beamable, Generic)
  primaryKey = NoteId . _noteId
