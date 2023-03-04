module Scheme.NoteDb (NoteDb(..)) where

import Database.Beam (TableEntity)
import Scheme.User (UserT)
import Scheme.Note (NoteT)

data NoteDb f
  = NoteDb
  { dbUsers :: f (TableEntity UserT)
  , dbNotes :: f (TableEntity NoteT)}
