module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (print)
import Data.Either (Either(..), isLeft, isRight)
import Data.Maybe (Maybe(..))
import Data.Tuple(Tuple(..))

import Data.URI
import Data.URI.Types
import Data.Path.Pathy

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

import Yesod.Auth

endpoint :: Endpoint
endpoint =
  { scheme: Just (URIScheme "http")
  , authority: Just (Authority Nothing [Tuple (NameAddress "localhost") (Just 7000)])
  , path: rootDir </> dir "auth" </> dir "page" </> dir "hardcoded"
  }

main = runTest do
  test "logging in with right credentials" do
    result <- login endpoint "Foo" "Foo"
    Assert.assert "Should be successful" $ isRight result 

  test "logging in with invalid credentials" do
    result <- login endpoint "Foo" "Bar"
    Assert.assert "Shouldn't be successful" $ isLeft result
    Assert.assert "Should be 'LoginInvalid'" $ result == Left LoginInvalid

  test "logging in with non-existing username" do
    result <- login endpoint "anonymous" "Bar"
    Assert.assert "Shouldn't be successful" $ isLeft result
    Assert.assert "Should be 'LoginUsernameNotFound'" $
      result == Left (LoginUsernameNotFound "anonymous")
