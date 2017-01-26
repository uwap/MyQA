{-# LANGUAGE RecordWildCards #-}
module Main where

import Protolude -- hiding ((<>))
import Dhall hiding (Text)
import Lucid
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Network.Wai
import Network.Wai.Parse
import Data.String (String, fromString)
import Options.Applicative hiding (auto)

import Types
import Pages
import Database
import Session

loadConfig :: Bool -> LText -> IO Config
loadConfig b path = input auto path & if b then detailed else identity

getSettings :: Config -> Settings
getSettings Config {..} = defaultSettings
                        & setPort (fromIntegral port)
                        & setHost (fromString $ toS host)

cli :: Parser (Bool, String)
cli = (,)
  <$> switch    (long "verbose" <> short 'v' <> help "Show detailed error messages")
  <*> strOption (long "config"  <> short 'c' <> help "The path to the config file" <> value "./config")

app :: Config -> SessionKey -> Application
app c k req respond = uncurry runPage $ route req
  where runPage :: Status -> Page -> IO ResponseReceived
        runPage s h = do
          pl <- fst <$> parseRequestBody lbsBackEnd req
          pp <- runReaderT (renderBST h) (PageData c req k pl)
          respond $ responseLBS s [] pp

main :: IO ()
main = do
  (verbose, path) <- execParser $ info (helper <*> cli) (fullDesc <> progDesc "Start the MyQA Webserver")
  conf <- loadConfig verbose (toS path)
  putText "Creating database tables..."
  createDatabase conf
  putText "Starting webserver..."
  let sett = getSettings conf
  runWith True sett (app conf)
