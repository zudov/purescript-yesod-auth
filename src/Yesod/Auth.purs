module Yesod.Auth
  ( Endpoint()
  , LoginSuccess(..)
  , LoginError(..)
  , login
  , login'
  , logout
  , logout'
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

import Data.URI
import Data.URI.Types
import Data.Path.Pathy

-- | Identifies the location of the used authentication plugin.
-- | Example:
-- |
-- |     -- http://localhost:4000/auth/page/hardcoded/
-- |     myEndpoint =
-- |       { scheme: Just (URIScheme "http"
-- |       , authority:
-- |           Just (Authority
-- |                   Nothing
-- |                   [ Tuple (NameAddress "localhost" (Just 4000)) ])
-- |       , path: rootDir </> dir "auth" </> dir "page" </> dir "hardcoded"
-- |       }
type Endpoint
  = { scheme :: Maybe URIScheme
    , authority :: Maybe Authority
    , path :: AbsDir Sandboxed
    }

runEndpoint :: Endpoint -> RelFile Sandboxed -> URI
runEndpoint { scheme, authority, path } file = 
  URI scheme
      (HierarchicalPart authority (Just (Left (path </> file))))
      Nothing
      Nothing

-- | Makes login query to the '/login' of the provided endpoint using given
-- | username/password. The result is returned, and the token is persisted in
-- | the session.
-- |
-- |     login myEndpoint "username" "password"
login
  :: ∀ eff.
     Endpoint
  -> String
  -> String
  -> Aff (ajax :: AJAX, dom :: DOM | eff) (Either LoginError LoginSuccess)
login endpoint = login' (printURI (runEndpoint endpoint (file "login")))

-- | This version of `login` allows to just provide URI of the '/login' endpoint
-- | as a `String`. No fancy types involved.
-- | 
-- |     login' "http://localhost:7000/auth/page/hardcoded/login" "username" "password"
login'
  :: ∀ eff.
     String -- ^ URI of the login endpoint
  -> String -- ^ Username
  -> String -- ^ Password
  -> Aff (ajax :: AJAX, dom :: DOM | eff) (Either LoginError LoginSuccess)
login' uri username password = 
   runLoginResponse <<< _.response <$> Ajax.post uri formquery
  where
    formquery =
      FormURLEncoded
        [ Tuple "username" (Just username)
        , Tuple "password" (Just password) ]

-- | Makes logout query to the '/logout' of the provided endpoint using given
-- | username/password.
-- |
-- |     logout myEndpoint
logout :: ∀ eff. Endpoint -> Aff (ajax :: AJAX | eff) Unit
logout endpoint = logout' (printURI (runEndpoint endpoint (file "logout")))

-- | This version of `logout` allows to just provide URI of the '/logout' endpoint
-- | as a `String`. No fancy types involved.
-- | 
-- |     logout' "http://localhost:7000/auth/page/hardcoded/logout"
logout' :: ∀ eff. String -> Aff (ajax :: AJAX | eff) Unit
logout' uri = _.response <$> Ajax.post' uri (Nothing :: Maybe Unit)

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
