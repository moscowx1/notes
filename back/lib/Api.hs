{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Auth (..),
  Api (..),
  Payload (..),
  Role (..),
  JwtHeader,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Dto.Auth (LoginReq, RegisterReq)
import Servant.API (
  Get,
  Header,
  Headers,
  JSON,
  NamedRoutes,
  NoContent,
  Post,
  ReqBody,
  (:-),
  (:>),
 )
import Servant.API.Generic (Generic)
import Servant.Auth (JWT)
import qualified Servant.Auth as SA
import Servant.Auth.JWT (FromJWT, ToJWT)
import Web.Cookie (SetCookie)

data Role = UserRole | AdminRole
  deriving (Generic, FromJSON, ToJSON)

data Payload = Payload {role :: Role, login :: Text}
  deriving (Generic, FromJSON, ToJSON, ToJWT, FromJWT)

type JwtHeader =
  ( Headers
      '[ Header "Set-Cookie" SetCookie
       , Header "Set-Cookie" SetCookie
       ]
      NoContent
  )

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
          :> Post '[JSON] JwtHeader
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
