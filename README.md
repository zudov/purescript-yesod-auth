# purescript-yesod-auth

Bindings to `yesod-auth` endpoints.

`test/` directory contains a runnable example. Start the server by executing
`./test/Main.hs` (requires `stack`) and launch `pulp test` to do some queries
against it.

Here is a glimse at the API:

```haskell
import Yesod.Auth (login, logout, LoginSuccess(..), LoginFailure(..))
import Yesod.Auth.Plugin (hardcoded)

let authRoute =
      { scheme: Just (URIScheme "http")
      , authority: Just (Authority Nothing [Tuple (NameAddress "missile.dog") Nothing])
      , path: rootDir </> dir "auth"
      }

result <- login authRoute hardcoded "username" "password"

case result of
  Right LoginSuccess
    -> do alert "Welcome!"

          alert "Now lets launch missiles"
          Ajax.post "http://missile.dog/launch" "Poehali"

          alert "Now we gonna log you out"
          logout authRoute

  Left (LoginUsernameNotFound username)
    -> alert ("We don't know of any " <> username)

  Left LoginInvalid
    -> alert "Your credentials are invalid, go away."

  Left (LoginError msg)
    -> alert ("Failed to login. " <> msg)

```

Yesod persists authentication using cookies, so credentials become a part of
implicit global state, keep that in mind and handle appropriately.
