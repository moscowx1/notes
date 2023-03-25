module App.Register (component) where

import Prelude

import Data.String (length)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = 
  { login :: String
  , password :: String
  , formValid :: Boolean
  }

data Action
  = LoginChange String
  | PasswordChange String
  | Submit

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { login: "", password: "", formValid: false }
    , render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction }
    }

handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  LoginChange l -> H.modify_ $ _ { login = l } >>> validate
  PasswordChange p -> do
    H.modify_ $  _ { password = p } >>> validate
  Submit -> H.modify_ identity

  where
    validate :: State -> State
    validate st = st { formValid = isValid st } 

    isValid :: State -> Boolean
    isValid st = length st.login > 3 && length st.password > 3

render :: forall cs m. State -> H.ComponentHTML Action cs m
render st =
  HH.form_
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
