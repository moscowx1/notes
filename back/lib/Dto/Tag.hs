{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Dto.Tag (CreateTagReq (..), SearchTagsReq (..)) where

import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype CreateTagReq = CreateTagReq
  { _value :: Text
  }
  deriving (Generic, FromJSON)

data SearchTagsReq = SearchTagsReq
  { _limit :: Int64
  , _searchPhrase :: Text
  }
  deriving (Generic, FromJSON)
