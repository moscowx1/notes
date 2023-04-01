{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api (Auth (..), Api (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Dto.Auth (LoginReq, RegisterReq)
import Servant.API (
  Get,
  JSON,
  NamedRoutes,
  Post,
  ReqBody,
  (:-),
  (:>),
 )
import Servant.API.Generic (Generic)
import Servant.Auth (JWT)
import qualified Servant.Auth as SA
import Servant.Auth.JWT (FromJWT, ToJWT)

data Role = User | Admin
  deriving (Generic, FromJSON, ToJSON)

data Payload = Payload {role :: Role, expiresdAt :: UTCTime, login :: String}
  deriving (Generic, FromJSON, ToJSON, ToJWT, FromJWT)

data Auth routes = Auth
  { _register ::
      routes
        :- "register"
          :> ReqBody '[JSON] RegisterReq
          :> Post '[JSON] String
  , _signIn ::
      routes
        :- "sign-in"
          :> ReqBody '[JSON] LoginReq
          :> Post '[JSON] String
  }
  deriving (Generic)

data Notes routes = Notes
  { _create ::
      routes
        :- "create"
          :> Post '[JSON] String
  , _get ::
      routes
        :- Get '[JSON] String
  }
  deriving (Generic)

data Api routes = Api
  { _auth :: routes :- "auth" :> NamedRoutes Auth
  , _notes :: routes :- SA.Auth '[JWT] Payload :> "notes" :> NamedRoutes Notes
  }
  deriving (Generic)
