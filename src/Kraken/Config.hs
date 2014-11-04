{-# LANGUAGE OverloadedStrings, TupleSections #-}


module Kraken.Config where


import           Control.Applicative
import           Data.Configurator
import           Data.Configurator.Types
import           Prelude                 hiding (lookup)


data KrakenConfig = KrakenConfig {
  retryDelay :: Maybe Double
 }
  deriving (Show, Eq, Ord)

defaultKrakenConfig :: KrakenConfig
defaultKrakenConfig = KrakenConfig {
  retryDelay = Nothing
 }

loadConfig :: FilePath -> IO (KrakenConfig, Config)
loadConfig configFile = do
  config <- load [Required configFile]
  let custom = subconfig "customConfig" config
  result <- KrakenConfig <$>
     lookup config "retryDelay"

  seq (length $ show result) (return ())
  return (result, custom)
