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

data Config = Config
              { allowSignup           :: Bool
              , port                  :: Integer
              , host                  :: LText
              , psql                  :: PSQLConfig
              } deriving (Generic, Show, Eq)
instance Interpret Config
