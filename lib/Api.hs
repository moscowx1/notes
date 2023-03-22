{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api (UserApi (..), Api (..)) where

import Dto.Auth (RegisterReq)
import Servant.API (
  JSON,
  NamedRoutes,
  Post,
  ReqBody,
  (:-),
  (:>),
 )
import Servant.API.Generic (Generic)

data UserApi routes = UserApi
  { _register ::
      routes
        :- "register"
        :> ReqBody '[JSON] RegisterReq
        :> Post '[JSON] Bool
        -- , _getAll :: routes :- "get-all" :> Get '[JSON] []
  }
  deriving (Generic)

data Api routes = Api
  { _user :: routes :- "user" :> NamedRoutes UserApi
  }
  deriving (Generic)
