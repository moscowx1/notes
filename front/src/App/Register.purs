module App.Register (component) where

import Prelude

import Api.Test (kk')
import Data.String (length)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow, log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State = 
  { login :: String
  , password :: String
  , formValid :: Boolean
  }

data Action
  = LoginChange String
  | PasswordChange String
  | Submit Event

component :: forall q i o m. MonadAff m =>  H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { login: "", password: "", formValid: false }
    , render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction }
    }

handleAction ::
  forall o m. MonadAff m =>
  Action ->
  H.HalogenM State Action () o m Unit
handleAction = case _ of
  LoginChange l -> H.modify_ $ _ { login = l } >>> validate
  PasswordChange p -> do
    H.modify_ $  _ { password = p } >>> validate
  Submit event -> do
    H.liftEffect $ log "asd123asd"
    H.liftEffect $ Event.preventDefault event
    resp <- H.liftAff kk'
    H.liftEffect $ logShow resp
    pure unit

  where
    validate :: State -> State
    validate st = st { formValid = isValid st } 

    isValid :: State -> Boolean
    isValid st = length st.login > 3 && length st.password > 3

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.form
    [ HE.onSubmit \e -> Submit e ]
    [ HH.h2_ [ HH.text "registration"]
    , HH.label_ [ HH.text "login" ]
    , HH.input
        [ HE.onValueInput \l -> LoginChange l
        , HP.type_ HP.InputText
        ]
    , HH.label_ [ HH.text "password" ]
    , HH.input
        [ HE.onValueInput \p -> PasswordChange p
        , HP.type_ HP.InputPassword
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit
        , HP.disabled $ not st.formValid
        ]
        [ HH.text "Submit" ]
    ]
