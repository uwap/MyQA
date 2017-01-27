module Pages.Common where

import Protolude
import Lucid
import Types
import Data.List (lookup)

lookupFormField :: ByteString -> SubPage (Maybe ByteString)
lookupFormField l = do
  f <- asks postFields
  return $ lookup l f

page_ :: Page -> Page
page_ p = doctypehtml_ $ do
            head_ $ do
              meta_ [ charset_ "utf-8" ]
              link_ [ rel_ "stylesheet", type_ "text/css", href_ "/style.css" ]
            body_ $ do
              nav_ $
                ul_ $ do
                  li_ $ with a_ [ href_ "/login" ] "Login"
                  li_ "World"
              with div_ [ id_ "content" ] p
              
page404 :: Page
page404 = page_ "The requested page does not exist"

page405 :: Page
page405 = page_ "The requested method is not available"
