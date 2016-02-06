module Yesod.Auth
  ( LoginSuccess(..)
  , LoginError(..)
  , login
  , logout
  ) where

import Control.Monad.Aff (Aff())
import DOM (DOM())
import Data.Maybe (Maybe(Just, Nothing))
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax (post, post') as Ajax
import Prelude (class Eq, class Show, Unit, (>>>), map, (<>), (<$>), (<<<))

import Data.Generic (class Generic, gShow, gEq)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String as String
import Data.Foreign.Class (readProp) as Foreign
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Network.HTTP.MimeType.Common as Mime

import Data.FormURLEncoded (FormURLEncoded(..))

-- | Takes url, username, password
login
  :: ∀ eff.
     String
  -> String
  -> String
  -> Aff (ajax :: AJAX, dom :: DOM | eff) (Either LoginError LoginSuccess)
login url username password =
  runLoginResponse <<< _.response <$> (Ajax.post (url <> "/login") formquery)
  where
    formquery =
      FormURLEncoded
        [ Tuple "username" (Just username)
        , Tuple "password" (Just password) ]

-- | Takes url
logout :: ∀ eff. String -> Aff (ajax :: AJAX | eff) Unit
logout url = _.response <$> Ajax.post' (url <> "/logout") (Nothing :: Maybe Unit)

newtype LoginResponse = LoginResponse (Either LoginError LoginSuccess)

runLoginResponse :: LoginResponse -> Either LoginError LoginSuccess
runLoginResponse (LoginResponse a) = a

data LoginSuccess
  = LoginSuccess

data LoginError
  = LoginError String
  | LoginUsernameNotFound String
  | LoginInvalid

derive instance genericLoginSuccess :: Generic LoginSuccess
instance showLoginSuccess :: Show LoginSuccess where show = gShow
instance eqLoginSuccess :: Eq LoginSuccess where eq = gEq

derive instance genericLoginError :: Generic LoginError
instance showLoginError :: Show LoginError where show = gShow
instance eqLoginError :: Eq LoginError where eq = gEq

derive instance genericLoginResponse :: Generic LoginResponse
instance showLoginResponse :: Show LoginResponse where show = gShow
instance eqLoginResponse :: Eq LoginResponse where eq = gEq

instance respondableLoginResponse :: Respondable LoginResponse where
  responseType = Tuple (Just Mime.applicationJSON) JSONResponse
  fromResponse = Foreign.readProp "message" >>> map (fromMessage >>> LoginResponse)
    where
      fromMessage "Login Successful" = Right LoginSuccess
      fromMessage "Invalid username/password combination" = Left LoginInvalid
      fromMessage msg =
        case String.stripPrefix "Login not found: " msg of
          Just username -> Left (LoginUsernameNotFound username)
          Nothing -> Left (LoginError msg)
