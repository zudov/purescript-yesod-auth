#!/usr/bin/env stack
-- stack --resolver lts-5.1 runghc --package yesod-auth
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Text (Text)
import Data.Aeson

import Yesod
import Yesod.Auth
import Yesod.Auth.Hardcoded
import Yesod.Auth.Message

data App = App

mkYesod "App" [parseRoutes|
/     HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
  approot = ApprootStatic "http://localhost:7000"
  authRoute _ = Just $ AuthR LoginR

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = return . Just . credsIdent

  loginDest _ = HomeR
  logoutDest _ = HomeR

  authPlugins _ = [authHardcoded]
  authenticate Creds{ credsPlugin = "hardcoded", ..} = do 
    exists <- doesUserNameExist credsIdent
    if exists
      then pure $ Authenticated credsIdent
      else pure $ UserError InvalidLogin

  maybeAuthId = lookupSession "_ID"

instance YesodAuthHardcoded App where
  -- If your password is the same as your username, you are good to go
  -- Unless you are anonymous
  validatePassword "anonymous" p = pure False
  validatePassword u p = pure (u == p)
  doesUserNameExist "anonymous" = pure False
  doesUserNameExist _ = pure True

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Value
getHomeR =
  maybeAuthId
    >>= maybe (permissionDenied "You aren't authenticated")
              (pure . String)

main = warp 7000 App 

