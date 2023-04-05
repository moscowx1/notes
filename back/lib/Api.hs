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
  Kek(..)
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Dto.Auth (LoginReq, RegisterReq)
import Servant.API (
  -- Get,
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
import Servant.Auth (Cookie, JWT)
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
          :> Post '[JSON] String
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

newtype Kek routes = Kek { _mol :: routes :- Post '[PlainText] String }
  deriving (Generic)

data Api routes = Api
  { _auth :: routes :- "auth" :> NamedRoutes Auth
  , _notes ::
      routes
        :- SA.Auth '[Cookie, JWT] Payload :> "notes" :> NamedRoutes Notes
  , _kek :: routes :- "kek" :> NamedRoutes Kek
  }
  deriving (Generic)
