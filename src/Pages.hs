module Pages where

import Protolude
import Lucid
import Clay (render)
import Network.HTTP.Types
import Network.Wai

import Types
import PageStyle
import Pages.Common
import Pages.UserView

index :: Page
index = page_ "hello world"

page404 :: Page
page404 = page_ "The requested page does not exist"

page405 :: Page
page405 = page_ "The requested method is not available"

route :: Request -> (Status, Page)
route r | requestMethod r == methodGet = routeGet
        | requestMethod r == methodPost = routePost
        | otherwise = (status405, page405)
  where
    routeGet = case pathInfo r of
      []            -> (status200, index)
      ["user",i]    -> (status200, userGet i)
      ["418"]       -> (status418, page_ "I'm a teapot")
      ["style.css"] -> (status200, toHtmlRaw (render style))
      _             -> (status404, page404)
    routePost = case pathInfo r of
      ["user",i] -> (status200, userPost i)
      _          -> (status404, page404)
