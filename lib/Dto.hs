{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Dto (UserDto(..), UnvalidatedCredential(..))  where
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

data UnvalidatedCredential = UnvalidatedCredential
  { _login :: Text
  , _password :: Text
  } deriving (Show, Eq, Generic, FromJSON)

data UserDto = UserDto
  { _login :: Text
  , _createdAt :: UTCTime
  } deriving (Show, Eq, Generic, ToJSON)
