module Test.Main where

import Prelude

import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, print)
import Data.Either (Either(..), isLeft, isRight)
import Data.Maybe (Maybe(..))
import Data.Tuple(Tuple(..))

import Data.URI
import Data.URI.Types
import Data.Path.Pathy

import Network.HTTP.Affjax (AJAX())
import Network.HTTP.Affjax as Ajax
import Network.HTTP.StatusCode

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

import Yesod.Auth (AuthRoute(), login, logout, LoginError(..), LoginSuccess(..))
import Yesod.Auth.Plugin (hardcoded)

assert msg bool = do
  when (not bool) $ liftEff $ log "SOME_TEST_HAS_FAILED"
  Assert.assert msg bool


authRoute :: AuthRoute
authRoute =
  { scheme: Just (URIScheme "http")
  , authority: Just (Authority Nothing [Tuple (NameAddress "localhost") (Just 7000)])
  , path: rootDir </> dir "auth"
  }

getHome :: ∀ eff. Aff (ajax :: AJAX | eff) (Either String String)
getHome = do
  { status, response } <- Ajax.get "http://localhost:7000/" :: Ajax.Affjax eff String
  pure case status of
    StatusCode 200 -> Right response
    _ -> Left response

main = runTest do
  test "logging in with invalid credentials" do
    result <- login authRoute hardcoded "Foo" "Bar"
    assert "Shouldn't be successful" $ isLeft result
    assert "Should be 'LoginInvalid'" $ result == Left LoginInvalid

  test "logging in with non-existing username" do
    result <- login authRoute hardcoded "anonymous" "Bar"
    assert "Shouldn't be successful" $ isLeft result
    assert "Should be 'LoginUsernameNotFound'" $
      result == Left (LoginUsernameNotFound "anonymous")

  test "logging in with valid credentials and then logging out" do
    result <- login authRoute hardcoded "Foo" "Foo"
    assert "Should be successful" $ isRight result 

    isLoggedIn <- isRight <$> getHome 
    assert "Should be logged in" isLoggedIn

    logout authRoute
    isLoggedIn <- isRight <$> getHome 
    assert "Shouldn't be logged in" (not isLoggedIn)

  liftEff $ log "TESTS_ARE_FINE"
