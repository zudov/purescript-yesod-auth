module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (print)

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

import Yesod.Auth

main = runTest do
  test "logging in with right credentials" do
    result <- login "http://localhost:7000/auth/page/hardcoded" "Foo" "Foo"
    Assert.assert "Should be successful" $ result == LoginSuccess

  test "logging in with wrong credentials" do
    result <- login "http://localhost:7000/auth/page/hardcoded" "Foo" "Bar"
    Assert.assert "Shouldn't be successful" $ result == LoginFailure

  test "logging in with non-existing username" do
    result <- login "http://localhost:7000/auth/page/hardcoded" "anonymous" "Bar"
    Assert.assert "Shouldn't be successful" $ result == LoginFailure
