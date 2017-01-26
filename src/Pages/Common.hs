module Pages.Common where

import Protolude
import Lucid
import Types
import Data.List (lookup)
import Network.Wai.Parse

lookupFormField :: ByteString -> SubPage (Maybe ByteString)
lookupFormField l = do
  r <- asks pageRequest
  liftIO $ lookup l . fst <$> parseRequestBody lbsBackEnd r

page_ :: Page -> Page
page_ p = doctypehtml_ $ do
            head_ $ do
              meta_ [ charset_ "utf-8" ]
              link_ [ rel_ "stylesheet", type_ "text/css", href_ "/style.css" ]
            body_ $ do
              nav_ $
                ul_ $ do
                  li_ "Hello"
                  li_ "World"
              with div_ [ class_ "content" ] p
