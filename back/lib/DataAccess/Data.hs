{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DataAccess.Data where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )
import Data.ByteString (ByteString)

type Salt = ByteString
type Login = Text
type HashedPassword = ByteString

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
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
