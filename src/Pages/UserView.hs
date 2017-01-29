module Pages.UserView where

import Protolude
import Lucid
import Database
import Pages.Common
import Types
import Session
import Utils

-----------------------------------------------------------------------
askForm :: SubPage ()
askForm = with form_ [ method_ "POST" ] $ do
  with textarea_ [ name_ "question" ] ""
  input_ [ type_ "submit", value_ "Ask!" ]

replyForm :: Integer -> SubPage ()
replyForm i = with form_ [ method_ "post" ] $ do
  input_ [ type_ "text", name_ "answer" ]
  input_ [ type_ "hidden", name_ "id", value_ (show i) ]
  input_ [ type_ "submit", value_ "Answer!" ]

showQuestion_ :: Text -> Text -> SubPage () -> SubPage ()
showQuestion_ question answer page =
  with div_ [ class_ "question" ] $ do
    h2_ $ toHtml question
    with p_ [ class_ "answer" ] $ toHtml answer
    page

showQuestion :: Text -> Text -> SubPage ()
showQuestion question answer = showQuestion_ question answer $ return ()

showQuestionAndReplyField :: Text -> Maybe Text -> Integer -> SubPage ()
showQuestionAndReplyField question manswer questionId =
  showQuestion_ question (fromMaybe "" manswer) $ replyForm questionId
-----------------------------------------------------------------------
userGet :: Text -> Page
userGet t = user_ t askForm 

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
                  (True, Just i) ->
                    ifM (isJust <$> addReply i (toS a))
                      (userGet t)
                      (user_ t "Something went wrong")
                  _              -> user_ t "Something went wrong"
              _ -> user_ t "Something went wrong"

user_ :: Text -> Page -> Page
user_ t p = do
  u <- getCookie UserID
  if fromMaybe "" u == toS t then do
    q <- questions t
    page_ $ p >> with div_ [ id_ "questions" ] (forM_ q (uncurry3 showQuestionAndReplyField))
  else do
    b <- isValidUser $ toS t
    if b then do
      q <- questionsWithReply t <&> map (\(x,y,_) -> (x,y))
      page_ $ p >> with div_ [ id_ "questions" ] (forM_ q (uncurry showQuestion))
    else page404
