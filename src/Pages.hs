module Pages where

import Protolude
import Lucid
import Clay (render)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse
import Data.List (lookup)

import Types
import Database
import PageStyle
import Session

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

index :: Page
index = page_ "hello world"

userGet :: SessionKey -> Request -> Config -> Text -> Page
userGet k r c t = user_ k r c t $ 
            with form_ [ method_ "POST" ] $ do
              with textarea_ [ name_ "question" ] ""
              input_ [ type_ "submit", value_ "Ask!" ]

userPost :: SessionKey -> Request -> Config -> Text -> Page
userPost k r c t = user_ k r c t $ do
            rt <- liftIO $ parseRequestBody lbsBackEnd r
            let question = lookup "question" $ fst rt
            case question of
              Just q -> do
                addQuestion c t $ toS q
                p_ "The question was added successfully"
              Nothing -> p_ "Something went wrong"

user_ :: SessionKey -> Request -> Config -> Text -> Page -> Page
user_ k r c t p = do
  u <- getCookie k (vault r) UserID
  if fromMaybe "" u == t then do
    q <- questions c t
    page_ $ p >> with div_ [ id_ "questions" ] (forM_ q showQuestionAndReplyField)
  else do
    q <- questionsWithReply c t
    page_ $ p >> with div_ [ id_ "questions" ] (forM_ q showQuestion)

showQuestionAndReplyField :: (Text,Maybe Text) -> Page
showQuestionAndReplyField (t,p) = with div_ [ class_ "question" ] $ do
                                    h2_ $ toHtml t
                                    with p_ [ class_ "answer" ] $ do
                                      toHtml (fromMaybe "" p)
                                      p_ "INSERT TEXT HERE"

showQuestion :: (Text,Text) -> Page
showQuestion (t, p) = with div_ [ class_ "question" ] $ do
                        h2_ $ toHtml t
                        with p_ [ class_ "answer" ] $ toHtml p

page404 :: Page
page404 = page_ "The requested page does not exist"

page405 :: Page
page405 = page_ "The requested method is not available"

route :: Config -> SessionKey -> Request -> (Status, Page)
route c k r | requestMethod r == methodGet = routeGet
            | requestMethod r == methodPost = routePost
            | otherwise = (status405, page405)
  where
    routeGet = case pathInfo r of
      []            -> (status200, index)
      ["user",i]    -> (status200, userGet k r c i)
      ["418"]       -> (status418, page_ "I'm a teapot")
      ["style.css"] -> (status200, toHtmlRaw (render style))
      _             -> (status404, page404)
    routePost = case pathInfo r of
      ["user",i] -> (status200, userPost k r c i)
      _          -> (status404, page404)
