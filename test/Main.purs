module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (print)
import Data.Either (Either(..), isLeft, isRight)

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

import Yesod.Auth

main = runTest do
  test "logging in with right credentials" do
    result <- login "http://localhost:7000/auth/page/hardcoded" "Foo" "Foo"
    Assert.assert "Should be successful" $ isRight result 

  test "logging in with invalid credentials" do
    result <- login "http://localhost:7000/auth/page/hardcoded" "Foo" "Bar"
    Assert.assert "Shouldn't be successful" $ isLeft result
    Assert.assert "Should be 'LoginInvalid'" $ result == Left LoginInvalid

  test "logging in with non-existing username" do
    result <- login "http://localhost:7000/auth/page/hardcoded" "anonymous" "Bar"
    Assert.assert "Shouldn't be successful" $ isLeft result
    Assert.assert "Should be 'LoginUsernameNotFound'" $
      result == Left (LoginUsernameNotFound "anonymous")
