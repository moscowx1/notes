module Migrations.Migration where

import Database.Beam.Migrate (MigrationSteps, CheckedDatabaseSettings)
import Database.Beam.Postgres (Postgres(Postgres))
import Migrations.V0001 (initialSetupStep, NoteDb)

fullMigration :: MigrationSteps Postgres
  ()
  (CheckedDatabaseSettings Postgres NoteDb)
fullMigration = initialSetupStep

