module Yesod.Auth.Plugin
  ( PluginId()
  , runPluginId
  , browserId
  , dummy
  , email
  , googleEmail2
  , hardcoded
  , openId
  , rpxnow
  , account
  , hashDB
  ) where

newtype PluginId
  = PluginId String

pluginId :: String -> PluginId
pluginId = PluginId

runPluginId :: PluginId -> String
runPluginId (PluginId a) = a

browserId :: PluginId
browserId = pluginId "browserid"

dummy :: PluginId
dummy = pluginId "dummy"

email :: PluginId
email = pluginId "email"

googleEmail2 :: PluginId
googleEmail2 = pluginId "googleemail2"

hardcoded :: PluginId
hardcoded = pluginId "hardcoded"

openId :: PluginId
openId = pluginId "openid"

rpxnow :: PluginId
rpxnow = pluginId "rpxnow"

account :: PluginId
account = pluginId "account"

hashDB :: PluginId
hashDB = pluginId "hashdb"
