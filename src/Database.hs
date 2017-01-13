{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Database where

import Protolude hiding ((<>))
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple
import Types

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

questions' :: MonadIO m => Config -> Text -> Query -> (forall r. FromRow r => m [r])
questions' c t q = liftIO . connectPQ c $ \con ->
  query con ("SELECT question, answer FROM questions WHERE user_id = (SELECT id FROM users WHERE username = ?)" <> q) (Only t) 

questions :: MonadIO m => Config -> Text -> m [(Text, Maybe Text)]
questions c user = questions' c user ""

questionsWithReply :: MonadIO m => Config -> Text -> m [(Text, Text)]
questionsWithReply c user = questions' c user " and answer is not null"
