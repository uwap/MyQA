{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database where

import Protolude hiding ((<>))
import Data.Monoid ((<>))
import Database.PostgreSQL.Simple
import Types

import Crypto.KDF.Scrypt hiding (p, r)
import Crypto.Random.Entropy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

scryptParams :: Parameters
scryptParams = Parameters 16384 8 1 512

createDatabase :: MonadIO m => Config -> m ()
createDatabase c = liftIO . connectPQ c $ \con -> do
    _ <- execute_ con ("CREATE TABLE IF NOT EXISTS users "
                    <> "( id SERIAL PRIMARY KEY"
                    <> ", username VARCHAR(64) NOT NULL UNIQUE"
                    <> ", password CHAR(1024) NOT NULL"
                    <> ", salt CHAR(64) NOT NULL"
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
    let q :: Connection -> IO [Only Int]
        q con = query con "SELECT count(*) FROM users WHERE username = ?" (Only u)
    (==1) . fromMaybe 0 . map fromOnly . headMay <$> connectDB q

isValidLogin :: ByteString -> ByteString -> SubPage Bool
isValidLogin u p =
  if BS.length u >= 64 then
    return False
  else connectDB $ \con -> do
    (r :: [(ByteString,ByteString)]) <- query con "SELECT password, salt FROM users WHERE username = ?" (Only u)
    case r of
      [(dp,s)] -> return $ B16.encode (generate scryptParams p s) == dp
      _        -> return False

createUser :: ByteString -> ByteString -> SubPage (Maybe SqlError)
createUser u p = connectDBe $ \con -> do
  salt <- B16.encode <$> getEntropy 32
  execute con "INSERT INTO users (username, password, salt, bio) VALUES (?,?,?,null)"
    (u, B16.encode $ generate scryptParams p salt, salt)
  return Nothing

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
addQuestion user question = void . connectDB $ \con ->
  execute con "INSERT INTO questions (question, answer, user_id) VALUES (?, null, (SELECT id FROM users WHERE username = ?))"
              (question, user)
