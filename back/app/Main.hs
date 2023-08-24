module Main (main) where

import Database.Beam (all_, runSelectReturningList, select)
import Database.Beam.Postgres (
  ConnectInfo (..),
  Connection,
  connect,
  runBeamPostgres,
 )
import Entities.User (NotesDb (..), User, notesDb)

conInfo :: ConnectInfo
conInfo =
  ConnectInfo
    { connectHost = "localhost"
    , connectPort = 5432
    , connectUser = "postgres"
    , connectPassword = "postgres"
    , connectDatabase = "notes"
    }

-- q :: Connection -> IO ()
q :: Connection -> IO [User]
q c = runBeamPostgres c $ do
  runSelectReturningList $ select (all_ (_users notesDb))

main :: IO ()
main = do
  c <- connect conInfo
  us <- q c
  print us
