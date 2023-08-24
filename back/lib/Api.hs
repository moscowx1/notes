{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Dto.Auth (RegisterReq)
import Servant (Header, Headers, JSON, NamedRoutes, NoContent, Post, ReqBody, (:-), (:>))
import Servant.API.Generic (Generic)
import Web.Cookie (SetCookie)

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
        :- "register"
          :> ReqBody '[JSON] RegisterReq
          :> Post '[JSON] JwtHeader
  }
  deriving (Generic)

newtype Api routes = Api
  { _auth :: routes :- "auth" :> NamedRoutes Auth
  }
  deriving (Generic)
