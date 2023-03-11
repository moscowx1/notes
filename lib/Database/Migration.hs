module Database.Migration (migrateDb) where

import Database.Beam.Migrate (MigrationSteps, CheckedDatabaseSettings)
import Database.Beam.Migrate.Simple 
  ( BringUpToDateHooks(runIrreversibleHook)
  , defaultUpToDateHooks
  , bringUpToDateWithHooks
  )
import Database.Beam.Postgres (Postgres, Connection, runBeamPostgresDebug)
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.Migrations.V0001 (initialSetupStep, NoteDb)

fullMigration :: MigrationSteps Postgres
  ()
  (CheckedDatabaseSettings Postgres NoteDb)
fullMigration = initialSetupStep

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }

migrateDb :: Connection
  -> IO (Maybe (CheckedDatabaseSettings Postgres NoteDb))
migrateDb conn = runBeamPostgresDebug putStrLn conn $
  bringUpToDateWithHooks
    allowDestructive
    migrationBackend
    fullMigration

