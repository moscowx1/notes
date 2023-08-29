{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Database.Entities.User (Login)
import Dto.Auth (Credential)
import Servant (Header, Headers, JSON, NamedRoutes, NoContent, Post, ReqBody, (:-), (:>))
import Servant.API.Generic (Generic)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Web.Cookie (SetCookie)

data Payload = Payload {login :: Login, userId :: Int32}
  deriving (Generic, FromJSON, ToJSON, ToJWT, FromJWT, Show, Eq)

type JwtSetter m a = Payload -> m (Maybe a)

type JwtHeader =
  ( Headers
      '[ Header "Set-Cookie" SetCookie
       , Header "Set-Cookie" SetCookie
       ]
      NoContent
  )

newtype Auth routes = Auth
  { _register ::
      routes
        :- "sign-up"
          :> ReqBody '[JSON] Credential
          :> Post '[JSON] JwtHeader
  }
  deriving (Generic)

newtype Api routes = Api
  { _auth :: routes :- "auth" :> NamedRoutes Auth
  }
  deriving (Generic)
