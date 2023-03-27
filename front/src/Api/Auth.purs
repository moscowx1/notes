module Api.Auth where

import Prelude

import Affjax (Request, URL, defaultRequest, request)
import Affjax.ResponseFormat (ResponseFormat(..))
import Affjax.Web (Error, Response, driver)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Halogen.HTML.Properties (method)

host :: String
host = "localhost:8080"

scheme :: String
scheme = "http://"

--makeReq :: forall a. Method -> URL -> Aff (Either Error (Response a))
--makeReq :: Method -> URL -> Aff (Either Error (Response Json))
--makeReq method url = request driver $ defaultRequest
--  { method = Left method
--  , url = url
--  , responseFormat = json
--  }


--register :: 