{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Entities.User where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam (Beamable, C, Generic, Identity, Table (..))

data UserT f = User
  { _userId :: C f Int32
  , _userLogin :: C f Text
  , _userSalt :: C f ByteString
  , _userPassword :: C f ByteString
  , _userCreatedAt :: C f LocalTime
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey :: UserT column -> PrimaryKey UserT column
  primaryKey = UserId . _userId

type UserId = PrimaryKey UserT Identity
deriving instance Show UserId
deriving instance Eq UserId
