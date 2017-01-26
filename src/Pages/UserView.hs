module Pages.UserView where

import Protolude
import Lucid
import Database
import Pages.Common
import Types
import Session

userGet :: Text -> Page
userGet t = user_ t $ 
            with form_ [ method_ "POST" ] $ do
              with textarea_ [ name_ "question" ] ""
              input_ [ type_ "submit", value_ "Ask!" ]

userPost :: Text -> Page
userPost t = user_ t $ do
            question <- lookupFormField "question"
            case question of
              Just q -> do
                addQuestion t $ toS q
                p_ "The question was added successfully"
              Nothing -> p_ "Something went wrong"

user_ :: Text -> Page -> Page
user_ t p = do
  u <- getCookie UserID
  if fromMaybe "" u == toS t then do
    q <- questions t
    page_ $ p >> with div_ [ id_ "questions" ] (forM_ q showQuestionAndReplyField)
  else do
    q <- questionsWithReply t
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

