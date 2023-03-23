{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api (Auth (..), Api (..)) where

import Dto.Auth (LoginReq, RegisterReq)
import Servant.API (
  JSON,
  NamedRoutes,
  Post,
  ReqBody,
  (:-),
  (:>),
 )
import Servant.API.Generic (Generic)

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

data Api routes = Api
  { _auth :: routes :- "auth" :> NamedRoutes Auth
  }
  deriving (Generic)
