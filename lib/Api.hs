{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api (UserApi(..), Api(..)) where

import Dto (UserDto, UnvalidatedCredential)
import Servant.API 
  ( Post
  , ReqBody
  , NamedRoutes
  , (:>), (:-)
  , JSON
  , Get)
import Servant.API.Generic (Generic)

data UserApi routes = UserApi
  { _register :: routes :- "register" 
      :> ReqBody '[JSON] UnvalidatedCredential
      :> Post '[JSON] Bool
  , _getAll :: routes :- "get-all" :> Get '[JSON] [UserDto]
  } deriving (Generic)

data Api routes = Api
  { _user :: routes :- "user" :> NamedRoutes UserApi
  } deriving (Generic)
