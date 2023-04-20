{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Auth (..),
  Api (..),
  Notes (..),
  Payload (..),
  Role (..),
  JwtHeader,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Dto.Auth (LoginReq, RegisterReq)
import Servant.API (
  Header,
  Headers,
  JSON,
  NamedRoutes,
  NoContent,
  PlainText,
  Post,
  ReqBody,
  (:-),
  (:>),
 )
import Servant.API.Generic (Generic)
import Servant.Auth (Cookie)
import qualified Servant.Auth as SA
import Servant.Auth.JWT (FromJWT, ToJWT)
import Web.Cookie (SetCookie)

data Role = UserRole | AdminRole
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

data Payload = Payload {role :: Role, login :: Text}
  deriving (Generic, FromJSON, ToJSON, ToJWT, FromJWT, Show, Eq)

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
          :> Post '[JSON] JwtHeader
  , _signIn ::
      routes
        :- "sign-in"
          :> ReqBody '[JSON] LoginReq
          :> Post '[JSON] JwtHeader
  }
  deriving (Generic)

newtype Notes routes = Notes
  { _get ::
      routes
        :- Post '[PlainText] String
  }
  deriving (Generic)

data Api routes = Api
  { _auth :: routes :- "auth" :> NamedRoutes Auth
  , _notes ::
      routes
        :- SA.Auth '[Cookie] Payload
          :> "notes"
          :> NamedRoutes Notes
  }
  deriving (Generic)
