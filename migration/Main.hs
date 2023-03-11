module Main where

import Config (readConnectInfo)
import Data.Functor ((<&>))
import Data.Ini (readIniFile)
import Database.Beam.Postgres (connect)
import Database.Migration (migrateDb)
import Utils (throwLeft)

main :: IO ()
main = do
  print "started migration"
  ini <- readIniFile "env.ini" <&> throwLeft
  let conInfo = throwLeft $ readConnectInfo ini
  print "config read"
  con <- connect conInfo
  print "connected to database"
  _ <- migrateDb con
  print "database migrated"

