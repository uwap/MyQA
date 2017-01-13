{-# LANGUAGE OverloadedStrings #-}
module Pages where

import Protolude
import Lucid
import Network.HTTP.Types

import Types
import Database

page_ :: Page -> Page
page_ p = doctypehtml_ $ do
            head_ $ meta_ [ charset_ "utf-8" ]
            body_ $ do
              nav_ $
                ul_ $ do
                  li_ "Hello"
                  li_ "World"
              with div_ [ class_ "content" ] p

index :: Page
index = page_ "hello world" 

user :: Config -> Text -> Page
user c t = do
  r <- questionsWithReply c t
  case r of
    [] -> page404
    _  -> page_ $ forM_ r onlyToPage

onlyToPage :: (Text,Text) -> Page
onlyToPage (t, p) = toHtml t >> toHtml p

page404 :: Page
page404 = page_ "The requested page does not exist"

route :: Config -> [Text] -> (Status, Page)
route c t = case t of
              []         -> (status200, index)
              ["user",i] -> (status200, user c i)
              _          -> (status404, page404)
