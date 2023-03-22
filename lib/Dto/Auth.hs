{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Dto.Auth (LoginReq, RegisterReq, Credential (..)) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Credential = Credential
  { login :: Text
  , password :: Text
  }
  deriving (Generic, FromJSON)

type LoginReq = Credential
type RegisterReq = Credential
