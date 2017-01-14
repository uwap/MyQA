{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Protolude hiding ((<>))
import Dhall hiding (Text)
import Lucid
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Network.Wai
import Data.String (String, fromString)
import Options.Applicative hiding (auto)

import Types
import Pages
import Database

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

app :: Config -> Application
app c req respond = uncurry runPage $ route c req
  where runPage :: Status -> Page -> IO ResponseReceived
        runPage s h = respond . responseLBS s [] =<< renderBST h

main :: IO ()
main = do
  (verbose, path) <- execParser $ info (helper <*> cli) (fullDesc <> progDesc "Start the MyQA Webserver")
  conf <- loadConfig verbose (toS path)
  putText "Creating database tables..."
  createDatabase conf
  putText "Starting webserver..."
  let sett = getSettings conf
  runSettings sett (app conf)
