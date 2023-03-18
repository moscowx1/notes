{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataAccess.Data where

import Database.Persist.TH 
  ( mkPersist
  , sqlSettings
  , mkMigrate
  , persistLowerCase
  , share
  )
import Data.Text (Text)
import Data.Time (UTCTime)
import Types (Login, Salt, HashedPassword)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  login Login
  salt Salt
  password HashedPassword
  createdAt UTCTime default=now()

  UniqueLogin login
  deriving Show Eq
Note
  content Text
  author UserId
  deriving Show
|]