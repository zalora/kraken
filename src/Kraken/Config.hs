{-# LANGUAGE DeriveGeneric, LambdaCase, OverloadedStrings, TupleSections #-}


module Kraken.Config where


import           Control.Applicative
import           Control.Exception   (ErrorCall (..), throwIO)
import qualified Data.ByteString     as BS
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Prelude             hiding (lookup)

data KrakenConfig = KrakenConfig {
  retryDelay :: Maybe Double,
  numberOfRetries :: Int
 }
  deriving (Show, Eq, Ord, Generic)

defaultKrakenConfig :: KrakenConfig
defaultKrakenConfig = KrakenConfig {
  retryDelay = Nothing,
  numberOfRetries = 1
 }

instance FromJSON KrakenConfig

data CustomConfig a = CustomConfig {
  customConfig :: a
 }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON a => FromJSON (CustomConfig a)

loadYAML :: FromJSON a => FilePath -> IO a
loadYAML file = BS.readFile file >>= return . decodeEither >>= \case
  Right config -> return config
  Left m -> throwIO . ErrorCall $
            "Kraken.Config.withConfig: decode failure ‘" ++ m ++ "’"

-- | Reads 'KrakenConfig' from file at given path as well as an extra
-- @customConfig@ object which is converted using the 'FromJSON'
-- instance.
--
-- Also see 'loadKrakenConfig'
loadConfig :: FromJSON a => FilePath -> IO (KrakenConfig, a)
loadConfig configFile = do
  krakenConfig <- loadYAML configFile
  CustomConfig customConfig <- loadYAML configFile
  return (krakenConfig, customConfig)

-- | As 'loadConfig' but ignores the possible extra object.
loadKrakenConfig :: FilePath -> IO KrakenConfig
loadKrakenConfig fp = fst <$> loadConf
  where
    -- Even though we do not use the subobject, we still need to
    -- specify which instance we want to use: technically in a lazy
    -- scenario, even () would do but that is unreliable in case
    -- loadConfig implementation changes and happens to force the
    -- value. Object is the most general thing we can expect and at
    -- worst case scenario we are forcing too much rather than
    -- crashing.
    loadConf :: IO (KrakenConfig, Value)
    loadConf = loadConfig fp
