{-# LANGUAGE OverloadedStrings #-}


module Kraken.Config where


import           Data.Configurator
import           Data.Configurator.Types


data KrakenConfig custom = KrakenConfig {
  customConfig :: Maybe custom
 }
  deriving (Show, Eq, Ord)

loadConfig :: (Show custom, Configured custom) =>
  FilePath -> IO (KrakenConfig custom)
loadConfig configFile = do
  config <- load [Required configFile]
  custom <- Data.Configurator.lookup config "customConfig"
  let result = KrakenConfig custom
  seq (length $ show result) (return ())
  return result
