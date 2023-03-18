{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Dto (UserDto, UserCredentialDto)  where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Text (Text)

data UserCredentialDto = CreateUserReq
  { login :: Text
  , password :: Text
  } deriving (Show, Eq, Generic, FromJSON)

data UserDto = UserDto
  { login :: Text
  , createdAt :: UTCTime
  } deriving (Show, Eq, Generic, ToJSON)
