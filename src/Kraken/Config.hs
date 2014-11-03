{-# LANGUAGE OverloadedStrings, TupleSections #-}


module Kraken.Config where


import           Data.Configurator
import           Data.Configurator.Types


data KrakenConfig = KrakenConfig
  deriving Show

defaultKrakenConfig :: KrakenConfig
defaultKrakenConfig = KrakenConfig

loadConfig :: FilePath -> IO (KrakenConfig, Config)
loadConfig configFile = do
  config <- load [Required configFile]
  let custom = subconfig "customConfig" config
      result = KrakenConfig
  seq (length $ show result) (return ())
  return (result, custom)
