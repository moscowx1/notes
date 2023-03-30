module App.Register (component) where

import Prelude

import Affjax (defaultRequest, printError)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (Error, Response, request)
import Control.Monad.RWS (gets, modify)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.String (length, null)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Creds =
  { login :: String
  , password :: String
  }

type State = 
  { creds :: Creds
  , formValid :: Boolean
  , isLoading :: Boolean
  , resultText :: String
  }

data Action
  = LoginChange String
  | PasswordChange String
  | Submit Event

encodeCred :: Creds -> Json
encodeCred = encodeJson

component :: forall q i o m. MonadAff m =>  H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> 
      { creds: { login: "", password: ""} 
      , formValid: false
      , isLoading: false
      , resultText: ""
      }
    , render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction }
    }

register :: Creds -> Aff (Either Error (Response String))
register cred = request defaultRequest
  { url = "http://" <> "localhost:8080" <> "/auth/register"
  , method = Left POST
  , content = Just $ RB.Json $ encodeCred cred
  , responseFormat = RF.string
  , headers = [ContentType $ MediaType "application/json"]
  }

handleAction ::
  forall o m. MonadAff m =>
  Action ->
  H.HalogenM State Action () o m Unit
handleAction = case _ of
  LoginChange l -> H.modify_ $ 
    _ { creds { login = l } } >>> validate
  PasswordChange p -> H.modify_ $ 
    _ { creds { password = p } } >>> validate
  Submit event -> do
    H.liftEffect $ Event.preventDefault event
    cred <- gets (_.creds)
    res <- withLoading $ H.liftAff $ register cred
    _ <- modify (_ { resultText = (show $ lmap printError res) })
    pure unit

  where
    withLoading fun= do
      _ <- modify $ _ { isLoading = true }
      res <- fun
      _ <- modify $ _ { isLoading = false }
      pure res

    validate :: State -> State
    validate st = st { formValid = isValid st.creds } 

    isValid :: Creds -> Boolean
    isValid cr = length cr.login > 3 && length cr.password > 3

renderIf ∷ forall w i. Boolean → HTML w i → HTML w i
renderIf cond el = if cond then el else HH.text ""

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.form
    [ HE.onSubmit \e -> Submit e ]
    [ HH.h2_ [ HH.text "registration"]
    , renderIf (not $ null $ st.resultText) 
      (HH.p_ [ HH.text st.resultText ])
    , HH.label_ [ HH.text "login" ]
    , HH.input
        [ HE.onValueInput \l -> LoginChange l
        , HP.type_ HP.InputText
        , HP.disabled st.isLoading
        ]
    , HH.label_ [ HH.text "password" ]
    , HH.input
        [ HE.onValueInput \p -> PasswordChange p
        , HP.type_ HP.InputPassword
        , HP.disabled st.isLoading
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit
        , HP.disabled $ not st.formValid || st.isLoading
        ]
        [ HH.text "Submit" ]
    ]
