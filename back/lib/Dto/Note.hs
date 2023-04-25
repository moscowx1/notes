{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dto.Note where

import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype GetNoteReq = GetNoteReq {_id :: Int64}
  deriving (Generic, FromJSON)

type CreateNoteReq = Text
