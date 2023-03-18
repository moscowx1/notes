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
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  login Text
  salt ByteString
  password ByteString
  createdAt UTCTime default=now()

  UniqueLogin login
  deriving Show
Note
  content Text
  author UserId
  deriving Show
|]