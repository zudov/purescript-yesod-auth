#!/usr/bin/env stack
-- stack --resolver lts-5.1 runghc --package yesod-auth
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Text (Text)

import Yesod
import Yesod.Auth
import Yesod.Auth.Hardcoded

data App = App

mkYesod "App" [parseRoutes|
/     HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
  approot = ApprootStatic "http://localhost:7000"

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = return . Just . credsIdent

  loginDest _ = HomeR
  logoutDest _ = HomeR

  authPlugins _ = [authHardcoded]

  maybeAuthId = lookupSession "_Id"

instance YesodAuthHardcoded App where
  -- If your password is the same as your username, you are good to go
  -- Unless you are anonymous
  validatePassword "anonymous" p = pure False
  validatePassword u p = pure (u == p)
  doesUserNameExist "anonymous" = pure False
  doesUserNameExist _ = pure True


instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]


main = do
  warp 7000 App 

