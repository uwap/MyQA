{-# LANGUAGE OverloadedStrings #-}
module Pages where

import Protolude
import Lucid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse
import Data.List (lookup)

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

userGet :: Config -> Text -> Page
userGet c t = user' c t $ 
            with form_ [ method_ "POST" ] $ do
              with textarea_ [ name_ "question" ] ""
              input_ [ type_ "submit", value_ "Ask!" ]

userPost :: Request -> Config -> Text -> Page
userPost r c t = user' c t $ do
            rt <- liftIO $ parseRequestBody lbsBackEnd r
            let question = lookup "question" $ fst rt
            case question of
              Just q -> do
                addQuestion c t $ toS q
                p_ "The question was added successfully"
              Nothing -> p_ "Something went wrong"

user' :: Config -> Text -> Page -> Page
user' c t p = do
  r <- questionsWithReply c t
  case r of
    [] -> page404
    _  -> page_ $ do
            p
            forM_ r showQuestion

showQuestion :: (Text,Text) -> Page
showQuestion (t, p) = with div_ [ class_ "question" ] $ do
                        h2_ $ toHtml t
                        with p_ [ class_ "answer" ] $ toHtml p

page404 :: Page
page404 = page_ "The requested page does not exist"

page405 :: Page
page405 = page_ "The requested method is not available"

route :: Config -> Request -> (Status, Page)
route c r | requestMethod r == methodGet = routeGet
          | requestMethod r == methodPost = routePost
          | otherwise = (status405, page405)
  where
    routeGet = case pathInfo r of
      []         -> (status200, index)
      ["user",i] -> (status200, userGet c i)
      ["418"]    -> (status418, page_ "I'm a teapot")
      _          -> (status404, page404)
    routePost = case pathInfo r of
      ["user",i] -> (status200, userPost r c i)
      _          -> (status404, page404)
