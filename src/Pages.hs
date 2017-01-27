module Pages where

import Protolude
import Lucid
import Network.HTTP.Types
import Network.Wai

import Types
import Pages.Common
import Pages.UserView
import Pages.LoginView

import qualified Data.ByteString as BS

index :: Page
index = page_ "hello world"

route :: Request -> (Status, Page)
route r | requestMethod r == methodGet = routeGet
        | requestMethod r == methodPost = routePost
        | otherwise = (status405, page405)
  where
    routeGet = case pathInfo r of
      []            -> (status200, index)
      ["login"]     -> (status200, loginGet)
      ["u",i]       -> (status200, userGet i)
      ["418"]       -> (status418, page_ "I'm a teapot")
      ["style.css"] -> (status200, toHtmlRaw =<< liftIO (readFile "static/style.css"))
      ["bg.png"]    -> (status200, toHtmlRaw =<< liftIO (BS.readFile "static/bg.png"))
      _             -> (status404, page404)
    routePost = case pathInfo r of
      ["u",i]    -> (status200, userPost i)
      ["login"]  -> (status200, loginPost)
      _          -> (status404, page404)
