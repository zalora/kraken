{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase #-}


module Kraken.Config where


import           Control.Applicative
import           Control.Exception (throwIO, ErrorCall(..))
import qualified Data.ByteString as BS
import           Data.Maybe
import           Data.Yaml
import           Prelude hiding (lookup)

data KrakenConfig = KrakenConfig {
  retryDelay :: Maybe Double,
  numberOfRetries :: Int
 }
  deriving (Show, Eq, Ord)

defaultKrakenConfig :: KrakenConfig
defaultKrakenConfig = KrakenConfig {
  retryDelay = Nothing,
  numberOfRetries = 1
 }

newtype ConfigPair a = ConfigPair { _unConfigPair :: (KrakenConfig, a) }
                     deriving (Eq, Show)

-- | The reason for this instance instead of using 'Generic' for
-- KrakenConfig is the type of 'numberOfRetries' field: the 'Generic'
-- instance will always expectan 'Int' and fail to parses if the field
-- is missing. This field is actually optional but defaults to 1 which
-- means we need a custom parser for the field and therefore a custom
-- instance.
instance FromJSON a => FromJSON (ConfigPair a) where
  parseJSON (Object v) = do
    delay <- v .: "retryDelay"
    retries <- v .:? "numberOfRetries" >>= return . fromMaybe 1
    custom <- v .: "customConfig"
    return $ ConfigPair (KrakenConfig delay retries, custom)

  parseJSON _ = fail "FromJSON.parseJSON failure for ConfigPair"

loadYAML :: FromJSON a => FilePath -> IO a
loadYAML file = BS.readFile file >>= return . decodeEither >>= \case
  Right config -> return config
  Left m -> throwIO . ErrorCall $
            "Kraken.Config.loadYAML: decode failure ‘" ++ m ++ "’"

-- | Reads 'KrakenConfig' from file at given path as well as an extra
-- @customConfig@ object which is converted using the 'FromJSON'
-- instance.
--
-- Also see 'loadKrakenConfig'
loadConfig :: FromJSON a => FilePath -> IO (KrakenConfig, a)
loadConfig configFile = loadYAML configFile >>= return . _unConfigPair

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
