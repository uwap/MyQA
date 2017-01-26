{-# LANGUAGE RankNTypes #-}
module Database where

import Protolude hiding ((<>))
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple
import Types

import qualified Data.ByteString as BS

createDatabase :: MonadIO m => Config -> m ()
createDatabase c = liftIO . connectPQ c $ \con -> do
    _ <- execute_ con ("CREATE TABLE IF NOT EXISTS users "
                    <> "( id SERIAL PRIMARY KEY"
                    <> ", username VARCHAR(64) NOT NULL"
                    <> ", password VARCHAR(512) NOT NULL"
                    <> ", bio TEXT"
                    <> ")")
    _ <- execute_ con ("CREATE TABLE IF NOT EXISTS questions "
                    <> "( id SERIAL PRIMARY KEY"
                    <> ", question TEXT NOT NULL"
                    <> ", answer TEXT"
                    <> ", user_id SERIAL"
                    <> ")")
    return ()

isValidUser :: ByteString -> SubPage Bool
isValidUser u =
  if BS.length u >= 64 then
    return False
  else do
    c <- asks globalConfig
    let q :: Connection -> IO [Only Text]
        q con = query con "SELECT username FROM users WHERE username = ?" (Only u)
    (>0) . length <$> liftIO (connectPQ c q)

isValidLogin :: ByteString -> ByteString -> SubPage Bool
isValidLogin u p =
  if BS.length u >= 64 then
    return False
  else do
    c <- asks globalConfig
    let q :: Connection -> IO [Only Text]
        q con = query con "SELECT username FROM users WHERE username = ? AND password = ?" (u,p)
    (>0) . length <$> liftIO (connectPQ c q)

createUser :: ByteString -> ByteString -> Page
createUser u p = void $ do
  c <- asks globalConfig
  liftIO . connectPQ c $ \con ->
    execute con "INSERT INTO users (username, password, bio) VALUES (?,?,null)" (u,p)

questions' :: MonadIO m => Config -> Text -> Query -> (forall r. FromRow r => m [r])
questions' c t q = liftIO . connectPQ c $ \con ->
  query con ("SELECT question, answer FROM questions WHERE user_id = (SELECT id FROM users WHERE username = ?)" <> q) (Only t) 

questions :: Text -> SubPage [(Text, Maybe Text)]
questions user = do
  c <- asks globalConfig
  questions' c user ""

questionsWithReply :: Text -> SubPage [(Text, Text)]
questionsWithReply user = do
  c <- asks globalConfig
  questions' c user " and answer is not null"

addQuestion :: Text -> Text -> SubPage ()
addQuestion user question = do
  c <- asks globalConfig
  liftIO . void . connectPQ c $ \con ->
    execute con "INSERT INTO questions (question, answer, user_id) VALUES (?, null, (SELECT id FROM users WHERE username = ?))"
              (question, user)
