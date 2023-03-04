{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Scheme.User (UserT(..), PrimaryKey(UserId)) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam 
  ( Beamable
  , Columnar
  , Generic
  , PrimaryKey
  , primaryKey
  , Table)

data UserT f
  = User
    { _userId :: Columnar f Int32
    , _userNick :: Columnar f Text
    } deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId
