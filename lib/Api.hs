{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api (UserApi (..), Api (..)) where

import Dto (UnvalidatedCredential, UserDto)
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

data UserApi routes = UserApi
  { _register ::
      routes
        :- "register"
          :> ReqBody '[JSON] UnvalidatedCredential
          :> Post '[JSON] Bool
  , _getAll :: routes :- "get-all" :> Get '[JSON] [UserDto]
  }
  deriving (Generic)

data Api routes = Api
  { _user :: routes :- "user" :> NamedRoutes UserApi
  }
  deriving (Generic)
