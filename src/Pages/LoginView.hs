module Pages.LoginView where

import Protolude
import Lucid
import Database
import Pages.Common
import Pages.UserView
import Types
import Session

import qualified Data.ByteString as BS

loginGet :: Page
loginGet = loginGet' $ return ()

loginGet' :: Page -> Page
loginGet' p = page_ $ p >> do
    with div_ [ id_ "login" ] $ do
      h2_ "Login"
      form_ [ method_ "post" ] $ do
        formbody
        input_ [ type_ "submit", value_ "Log In!", name_ "login" ]
    with div_ [ id_ "signin" ] $ do
      h2_ "Sign In"
      form_ [ method_ "post" ] $ do
        formbody
        input_ [ type_ "submit", value_ "Sign In!", name_ "signin" ]
  where formbody = do
          "Username: "
          input_ [ type_ "text", name_ "username" ]
          br_ []
          "Password: "
          input_ [ type_ "password", name_ "password" ]
          br_ []

loginPost :: Page
loginPost = do
    signin <- lookupFormField "signin"
    login  <- lookupFormField "login"
    user   <- lookupFormField "username" & map justEmptyToNothing
    pw     <- lookupFormField "password" & map justEmptyToNothing
    case (signin, login, user, pw) of
      (Just _, Nothing, Just u, Just p) -> signin' u p
      (Nothing, Just _, Just u, Just p) -> login' u p 
      (_, _, _, _) -> error_ "Please enter username and password"
  where error_ x = loginGet' $ with p_ [ class_ "error" ] x
        justEmptyToNothing x = if fromMaybe True (BS.null <$> x) then Nothing else x
        login' u p = do
          x <- isValidLogin u p
          if x then success' u
          else error_ "Wrong username or password"
        signin' u p =
          if BS.length u >= 64 then
            error_ "The username may not be longer than 63 characters"
          else createUser u p >> success' u
        success' u = do
          setCookie UserID u
          userGet (toS u)
