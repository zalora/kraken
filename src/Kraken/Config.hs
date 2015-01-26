{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Kraken.Config where


import           Control.Applicative ((<$>))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Text (Text, unpack)
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

load :: FilePath -> IO Object
load p = BS.readFile p >>= return . decodeEither >>= \case
  Right o -> return o
  Left m -> error $ "Kraken.Config.load: decode failure ‘" ++ m ++ "’"

data LookupError = KeyError String | DecodeError String deriving (Show, Eq)

-- | Like 'lookupEither' but 'error's out if decoding or lookup fails.
lookup :: FromJSON a => Text -> Object -> a
lookup k o = case lookupEither k o of
  Right v -> v
  Left (DecodeError s) ->
    error $ "Kraken.Config.lookup: decode error ‘" ++ s ++ "’"
  Left (KeyError k') ->
    error $ "Kraken.Config.lookup: key ‘" ++ k' ++ "’ not found"

-- | Turn the 'Value' at given key of given object into something
-- nicer to use using its 'FromJSON' instance. 'error's out if
-- conversion fails.
lookupEither :: forall a. FromJSON a => Text -> Object -> Either LookupError a
lookupEither k o = case HM.lookup k o of
  Just x -> case A.fromJSON x :: A.Result a of
    A.Success v -> Right v
    A.Error s -> Left $ DecodeError s
  _ -> Left . KeyError $ unpack k

-- | Reads 'KrakenConfig' from file at given path as well as an extra
-- @customConfig@ object which is converted using the 'FromJSON'
-- instance.
--
-- Also see 'loadKrakenConfig'
loadConfig :: FromJSON a => FilePath -> IO (KrakenConfig, a)
loadConfig configFile = do
  config <- load configFile
  let retries = case lookupEither "numberOfRetries" config of
        Left _ -> 1
        Right x -> x

      result = KrakenConfig (lookup "retryDelay" config) retries

  seq (length $ show result) (return ())
  return (result, lookup "customConfig" config)


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
    loadConf :: IO (KrakenConfig, Object)
    loadConf = loadConfig fp
