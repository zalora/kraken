{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Web.Config where


import           Control.Exception
import           Data.String.Conversions
import           Data.Yaml
import           GHC.Generics
import           Network.URI
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           System.Environment


data Config = Config {
  port :: Port,
  krakenUri :: URI
 }
   deriving (Eq, Show, Generic)

instance ToJSON Config

instance FromJSON Config

instance ToJSON URI where
  toJSON uri = String $ cs $ show uri

instance FromJSON URI where
  parseJSON (String s) = case parseURI (cs s) of
    Nothing -> fail ("invalid uri: " ++ cs s)
    Just uri -> return uri
  parseJSON v = fail ("expected: string, got: " ++ cs (encode v))


loadConfig :: IO Config
loadConfig = do
  progName <- getProgName
  configFile <- execParser $ info (helper <*> options)
    (fullDesc <>
     progDesc "starts the webservice" <>
     header (progName ++ ": webservice (for humans) to observe kraken daemons"))
  eConfig <- decodeFileEither configFile
  either
    (throwIO . ErrorCall . show)
    return
    eConfig

options :: Options.Applicative.Parser FilePath
options = strOption
  (long "config" <>
   short 'c' <>
   metavar "FILE" <>
   help "configuration file" <>
   value "kraken-web.conf")
