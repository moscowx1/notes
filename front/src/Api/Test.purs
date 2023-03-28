module Api.Test where

import Prelude

import Affjax (Error(..), defaultRequest, post, printError)
import Affjax.RequestBody (RequestBody(..), string)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Error, Response, request)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
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

derive instance genM :: Generic M

testM :: M
testM = { login: "aasd", password: "ASd" }

--mm ∷ forall a. Show a => Aff (Either Error a)
--mm ∷ Aff (Either Error { body ∷ Unit , headers ∷ Array ResponseHeader , status ∷ StatusCode , statusText ∷ String } )
mm ∷ Aff (Either Error (Response String))
mm = request $ defaultRequest 
  { url =  url "/auth/sign-in"
  , responseFormat = RF.string
  , method = Left POST
  , content = RB.Json testM
  }

mm' = (lmap printError) <$> mm

kek :: forall a. Either Error (Response a) -> Either String (Response a)
kek = lmap printError


k :: Aff Unit
k = do
  res <- mm
  logShow (kek res)
  pure unit

lol = launchAff_ k