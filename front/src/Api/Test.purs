module Api.Test where

import Prelude

import Affjax (Error(..), defaultRequest, post, printError)
import Affjax.RequestBody (RequestBody(..), string)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Error, Response, Request, request)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Web.HTML.Event.EventTypes (offline)

host = "localhost:8080"

url path = "http://" <> host <> path

type M = 
  { login :: String
  , password :: String
  }

newtype Creds = Creds
  { login :: String
  , password :: String
  }

type Cred =
  { login :: String
  , password :: String
  }


--mm ∷ forall a. Show a => Aff (Either Error a)
--mm ∷ Aff (Either Error { body ∷ Unit , headers ∷ Array ResponseHeader , status ∷ StatusCode , statusText ∷ String } )
mm ∷ Aff (Either Error (Response String))
mm = request $ defaultRequest 
  { url =  url "/auth/sign-in"
  , responseFormat = RF.string
  , method = Left POST
  }

cred2 :: Cred -> Json
cred2 = encodeJson

kk :: Cred -> Request String
kk m = defaultRequest
  { url = url "/auth/sign-in"
  , method = Left POST
  , content = Just $ RB.Json $ cred2 m
  , responseFormat = RF.string
  , headers = [ContentType $ MediaType "application/json"]
  }

kk'' :: Cred -> Aff (Either Error (Response String))
kk'' m = request $ kk m

mm' = (lmap printError) <$> mm

kk' = (lmap printError) <$> (kk'' $ { login: "1234", password: "Asd"})

kek :: forall a. Either Error (Response a) -> Either String (Response a)
kek = lmap printError


k :: Aff Unit
k = do
  res <- mm
  logShow (kek res)
  pure unit

lol = launchAff_ k