{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Config (Config(..))
import Data.Aeson(eitherDecodeFileStrict)
import Data.Functor ((<&>))
import Database.Beam.Postgres (connect)
import Database.Migration (migrateDb)
import Utils (throwLeft)

main :: IO ()
main = do
  print "started migration"
  Config { _dbConnect }<- eitherDecodeFileStrict "config.json" <&> throwLeft
  print "config read"
  con <- connect _dbConnect
  print "connected to database"
  _ <- migrateDb con
  print "database migrated"

