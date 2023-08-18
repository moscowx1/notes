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
import DataAccess.Data (Note, Tag)
import Dto.Auth (LoginReq, RegisterReq)
import Dto.Note (CreateNoteReq, GetNoteReq)
import Dto.Tag (CreateTagReq, SearchTagsReq)
import Servant.API (
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

data Notes routes = Notes
  { _get ::
      routes
        :- ReqBody '[JSON] GetNoteReq
          :> Post '[JSON] Note
  , _create ::
      routes
        :- ReqBody '[JSON] CreateNoteReq
          :> Post '[JSON] Note
  }
  deriving (Generic)

data Tags routes = Tags
  { _createTag ::
      routes
        :- ReqBody '[JSON] CreateTagReq
          :> Post '[JSON] Tag
  , _searchTag ::
      routes
        :- ReqBody '[JSON] SearchTagsReq
          :> Post '[JSON] [Tag]
  }

data Api routes = Api
  { _auth :: routes :- "auth" :> NamedRoutes Auth
  , _notes ::
      routes
        :- SA.Auth '[Cookie] Payload
          :> "notes"
          :> NamedRoutes Notes
  , _session ::
      routes
        :- SA.Auth '[Cookie] Payload
          :> "session"
          :> Post '[JSON] Payload
  , _tags ::
      routes
        :- SA.Auth '[Cookie] Payload
          :> "tags"
          :> NamedRoutes Tags
  }
  deriving (Generic)
