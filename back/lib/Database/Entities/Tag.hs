{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Entities.Tag where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam (Beamable, C, Generic, Identity, Table (..))
import Database.Entities.User (UserT)

data TagT f = Tag
  { _tagId :: C f Int32
  , _tagValue :: C f Text
  , _tagAuthor :: PrimaryKey UserT f
  , _tagCreatedAt :: C f LocalTime
  }
  deriving (Generic, Beamable)

type Tag = TagT Identity

deriving instance Show Tag

deriving instance Eq Tag

instance Table TagT where
  data PrimaryKey TagT f = TagId (C f Int32)
    deriving (Generic, Beamable)
  primaryKey :: TagT column -> PrimaryKey TagT column
  primaryKey = TagId . _tagId

type TagId = PrimaryKey TagT Identity
deriving instance Show TagId
deriving instance Eq TagId
