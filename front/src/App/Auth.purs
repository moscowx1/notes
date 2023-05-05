module App.Auth where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, preventDefault)

data AuthType = Login | Register

data Action
  = LoginChange String
  | PasswordChange String
  | Submit Event
  | ChangeType

type State =
  { type :: AuthType
  , login :: String
  , password :: String
  , isLoading :: Boolean
  }

component :: forall q i o m. MonadAff m =>  H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ ->
      { type: Login
      , login: ""
      , password: ""
      , isLoading: false
      }
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction}
    }

handleAction ::
  forall o m. MonadAff m =>
  Action ->
  H.HalogenM State Action () o m Unit
handleAction = case _ of
  LoginChange l -> H.modify_  _ { login = l }
  PasswordChange p -> H.modify_ _ { password = p }
  ChangeType -> H.modify_ \st -> case st.type of
    Login -> st { type = Register }
    Register -> st { type = Login }
  Submit e -> do
    H.liftEffect (preventDefault e)
    H.modify_ _ { isLoading = true }
    pure unit

headerTxt :: AuthType -> String
headerTxt Login = "Login"
headerTxt Register = "Register"

changeAuthTxt :: AuthType -> String
changeAuthTxt Login = "register instead"
changeAuthTxt Register = "login instead"

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st = 
  HH.form
    [ HE.onSubmit \e -> Submit e]
    [ HH.h1_ [ HH.text (headerTxt st.type)]
    , HH.input
        [ HE.onValueInput \l -> LoginChange l
        , HP.type_ HP.InputText
        , HP.disabled st.isLoading
        ]
    , HH.input
        [ HE.onValueInput \p -> PasswordChange p
        , HP.type_ HP.InputPassword
        , HP.disabled st.isLoading
        ]
    , HH.button
        [ HP.disabled st.isLoading
        , HP.type_ HP.ButtonButton
        , HE.onClick \_ -> ChangeType]
        [ HH.text (changeAuthTxt st.type)]
    , HH.button
        [ HP.type_ HP.ButtonSubmit
        , HP.disabled st.isLoading
        ]
        [ HH.text "submit"]
    ]