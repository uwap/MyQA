{-# LANGUAGE FlexibleContexts #-}
module Session where

import Web.ClientSession
import Web.Cookie
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Session
import Network.Wai.Session.ClientSession
import qualified Data.Vault.Lazy as V

import Protolude
import Types

type SessionKey = V.Key (Session SubPage ByteString ByteString)

data CookieType = UserID

toBS :: CookieType -> ByteString
toBS UserID = "u"

getCookie :: StringConv ByteString s => SessionKey -> V.Vault -> CookieType -> SubPage (Maybe s)
getCookie k v t = case fst <$> V.lookup k v of
  Just f  -> map toS <$> f (toBS t)
  Nothing -> putText "FATAL ERROR. The key to the vault doesn't seem to work." >> return Nothing

setCookie :: SessionKey -> V.Vault -> CookieType -> ByteString -> SubPage ()
setCookie k v t c = case snd <$> V.lookup k v of
  Just f  -> f (toBS t) c
  Nothing -> putText "FATAL ERROR. The key to the vault doesn't seem to work."

cookieSettings :: Bool -> SetCookie
cookieSettings dev = def { setCookieHttpOnly = True
                         , setCookieSecure   = not dev
                         }

createSessionStore :: IO (SessionStore SubPage ByteString ByteString)
createSessionStore = clientsessionStore <$> getDefaultKey

runWith :: Bool -> Settings -> (SessionKey -> Application) -> IO ()
runWith dev r app = do
  session <- V.newKey
  store <- createSessionStore
  runSettings r $ withSession store "SESSION" (cookieSettings dev) session (app session)
