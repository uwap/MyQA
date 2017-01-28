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
userPost t = do
            question <- lookupFormField "question"
            answer   <- lookupFormField "answer"
            case (question, answer) of
              (Just q, _) -> do
                addQuestion t $ toS q
                user_ t "The question was added successfully"
              (_, Just a) -> do
                u <- map (==toS t) <$> getCookie UserID
                id <- (>>= map fst . headMay . reads . toS) <$> lookupFormField "id"
                case (fromMaybe False u,id) of
                  (True, Just i) -> do
                    addReply i (toS a)
                    userGet t
                  _              -> user_ t "Something went wrong"
              _ -> user_ t "Something went wrong"

user_ :: Text -> Page -> Page
user_ t p = do
  u <- getCookie UserID
  if fromMaybe "" u == toS t then do
    q <- questions t
    page_ $ p >> with div_ [ id_ "questions" ] (forM_ q showQuestionAndReplyField)
  else do
    b <- isValidUser $ toS t
    if b then do
      q <- questionsWithReply t
      page_ $ p >> with div_ [ id_ "questions" ] (forM_ q showQuestion)
    else page404

showQuestionAndReplyField :: (Text,Maybe Text,Integer) -> Page
showQuestionAndReplyField (t,p,i) = 
  with div_ [ class_ "question" ] $ do
    h2_ $ toHtml t
    with p_ [ class_ "answer" ] $ do
      toHtml (fromMaybe "" p)
      with form_ [ method_ "post" ] $ do
        input_ [ type_ "text", name_ "answer" ]
        input_ [ type_ "hidden", name_ "id", value_ (show i) ]
        input_ [ type_ "submit", value_ "Answer!" ]

showQuestion :: (Text,Text,a) -> Page
showQuestion (t,p,_) =
  with div_ [ class_ "question" ] $ do
    h2_ $ toHtml t
    with p_ [ class_ "answer" ] $ toHtml p

