module Yesod.Auth
  ( module Yesod.Auth.Types
  , login
  , logout
  ) where

import Data.Either (either)
import Control.Bind ((=<<))
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Eff (runPure)
import Control.Monad.Eff.Class (liftEff)
import Data.Tuple (Tuple(..))
import DOM (DOM())
import Data.Maybe (Maybe(Just, Nothing))
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax (post, post') as Ajax
import Prelude (($), (<$>), (<<<), const, Unit, (<>), (>>=), bind, pure)
import Yesod.Auth.Types (LoginResponse(..))

import Data.FormURLEncoded (FormURLEncoded(..))

-- | Takes url, username, password
login :: ∀ eff. String -> String -> String -> Aff (ajax :: AJAX, dom :: DOM | eff) LoginResponse
login url username password =
  _.response <$> (Ajax.post (url <> "/login") formquery)
  where
    formquery =
      FormURLEncoded
        [ Tuple "username" (Just username)
        , Tuple "password" (Just password) ]

-- | Takes url
logout :: ∀ eff. String -> Aff (ajax :: AJAX | eff) Unit
logout url = _.response <$> Ajax.post' (url <> "/logout") (Nothing :: Maybe Unit)

module Yesod.Auth.Types
  ( LoginResponse(..)
  ) where

import Prelude (class Show, (>>>), map, class Eq)
import Data.Generic (class Generic, gShow, gEq)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Foreign.Class (readProp) as Foreign
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Network.HTTP.MimeType.Common as Mime

data LoginResponse
  = LoginSuccess
  | LoginFailure

derive instance genericLoginResponse :: Generic LoginResponse
instance showLoginResponse :: Show LoginResponse where show = gShow
instance eqLoginResponse :: Eq LoginResponse where eq = gEq

instance respondableLoginResponse :: Respondable LoginResponse where
  responseType = Tuple (Just Mime.applicationJSON) JSONResponse
  fromResponse = Foreign.readProp "message" >>> map fromMessage
    where
      fromMessage "Login Successful" = LoginSuccess
      fromMessage _                  = LoginFailure
