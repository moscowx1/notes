module Migrations.Migration where

import Database.Beam.Migrate (MigrationSteps, CheckedDatabaseSettings)
import Database.Beam.Postgres (Postgres, Connection, runBeamPostgresDebug)
import Migrations.V0001 (initialSetupStep, NoteDb)
import Database.Beam.Migrate.Simple 
  ( BringUpToDateHooks(runIrreversibleHook)
  , defaultUpToDateHooks
  , bringUpToDateWithHooks
  )
import Database.Beam.Postgres.Migrate (migrationBackend)

fullMigration :: MigrationSteps Postgres
  ()
  (CheckedDatabaseSettings Postgres NoteDb)
fullMigration = initialSetupStep

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }

migrateDB :: Connection
  -> IO (Maybe (CheckedDatabaseSettings Postgres NoteDb))
migrateDB conn = runBeamPostgresDebug putStrLn conn $
  bringUpToDateWithHooks
    allowDestructive
    migrationBackend
    fullMigration
