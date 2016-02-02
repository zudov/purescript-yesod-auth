module Yesod.Auth
  ( module Yesod.Auth.Types
  , login
  , logout
  ) where

import Data.Either (either)
import Control.Monad.Aff (Aff(), attempt)
import Control.Monad.Eff (runPure)
import DOM.XHR.FormData as FormData
import Data.Maybe (Maybe(Just, Nothing))
import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax (post, post') as Ajax
import Prelude ((<$>), (<<<), const, Unit, (<>), (>>=))

import Yesod.Auth.Types (LoginResponse(..))

-- | Takes url, username, password
login :: ∀ eff. String -> String -> String -> Aff (ajax :: AJAX | eff) LoginResponse
login url username password =
  _.response <$> Ajax.post url (runPure formData)
  where
    formData = FormData.empty
           >>= FormData.insert "password" password
           >>= FormData.insert "username" username

logout :: ∀ eff. Aff (ajax :: AJAX | eff) Unit
logout = _.response <$> Ajax.post' "backend/auth/logout" (Nothing :: Maybe Unit)

module Yesod.Auth.Types
  ( LoginResponse(..)
  ) where

import Prelude (class Show, (>>>), map)
import Data.Generic (class Generic, gShow)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Foreign.Class (readProp) as Foreign
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..)) as Ajax
import Network.HTTP.MimeType.Common as Mime

data LoginResponse
  = LoginSuccess
  | LoginFailure

derive instance genericLoginResponse :: Generic LoginResponse
instance showLoginResponse :: Show LoginResponse where show = gShow

instance respondableLoginResponse :: Ajax.Respondable LoginResponse where
  responseType = Tuple (Just Mime.applicationJSON) Ajax.JSONResponse
  fromResponse = Foreign.readProp "message" >>> map fromMessage
    where
      fromMessage "Login Successful" = LoginSuccess
      fromMessage _                  = LoginFailure
