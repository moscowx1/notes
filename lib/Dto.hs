{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Dto (UserDto(..), UnvalidatedCredential(..))  where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Text (Text)

data UnvalidatedCredential = UnvalidatedCredential
  { _login :: Text
  , _password :: Text
  } deriving (Show, Eq, Generic, FromJSON)

data UserDto = UserDto
  { _login :: Text
  , _createdAt :: UTCTime
  } deriving (Show, Eq, Generic, ToJSON)
