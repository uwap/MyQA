{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Types where

import Protolude
import Dhall hiding (Text)
import Lucid
import Network.Wai
import Network.Wai.Session
import Database.PostgreSQL.Simple
import qualified Data.Vault.Lazy as V

type SessionKey = V.Key (Session SubPage ByteString ByteString)

type SubPage = HtmlT (ReaderT PageData IO)
type Page    = SubPage ()

data PageData = PageData
              { globalConfig :: Config
              , pageRequest  :: Request
              , sessionKey   :: SessionKey
              , postFields   :: [(ByteString, ByteString)]
              }

data PSQLConfig = PSQLConfig
                { psqlhost  :: LText
                , psqlport  :: Integer
                , psqluser  :: LText
                , password  :: LText
                , database  :: LText
                } deriving (Generic, Show, Eq)
instance Interpret PSQLConfig

toPQConInfo :: PSQLConfig -> ConnectInfo
toPQConInfo PSQLConfig {..} = ConnectInfo (toS psqlhost) (fromInteger psqlport) (toS psqluser) (toS password) (toS database)

connectPQ :: Config -> (Connection -> IO a) -> IO a
connectPQ (toPQConInfo . psql -> conInfo) = bracket (connect conInfo) close

connectDB :: (Connection -> IO a) -> SubPage a
connectDB f = asks globalConfig >>= liftIO . flip connectPQ f

connectDBe :: (Connection -> IO (Maybe SqlError)) -> SubPage (Maybe SqlError)
connectDBe f = do
  c <- asks globalConfig
  liftIO $ catch (connectPQ c f) (return . Just)

data Config = Config
              { allowSignup           :: Bool
              , frontPage             :: Maybe LText
              , port                  :: Integer
              , host                  :: LText
              , psql                  :: PSQLConfig
              } deriving (Generic, Show, Eq)
instance Interpret Config
