{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.Web.Config where


import           Control.Exception
import           Data.String.Conversions
import           Data.Yaml
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant
import           System.Environment


data Config = Config {
  port :: Port,
  krakenUris :: [BaseUrl]
 }
   deriving (Eq, Show, Generic)

instance ToJSON Config

instance FromJSON Config

instance ToJSON BaseUrl where
  toJSON = String . cs . showBaseUrl

instance FromJSON BaseUrl where
  parseJSON (String s) = case parseBaseUrl (cs s) of
    Left error -> fail (cs error)
    Right url -> return url
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
