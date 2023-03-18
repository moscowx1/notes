{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api (UserApi(..), Api(..)) where

import Dto (UserCredentialDto, UserDto)
import Servant.API 
  ( Post
  , ReqBody
  , NamedRoutes
  , (:>), (:-)
  , JSON
  , Get)
import Servant.API.Generic (Generic)


data UserApi routes = UserApi
  { _create :: routes :- "create" :> ReqBody '[JSON] UserCredentialDto
      :> Post '[JSON] (Maybe UserDto)
  , _getAll :: routes :- "get-all" :> Get '[JSON] [UserDto]
  } deriving (Generic)

data Api routes = Api
  { _user :: routes :- "user" :> NamedRoutes UserApi
  } deriving (Generic)