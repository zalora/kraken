

module Kraken.ConfigSpec where


import           Control.Applicative
import           Test.Hspec

import           Kraken.Config


spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads the example config" $ do
      (fst <$> loadConfig "kraken.conf.example") `shouldReturn` (KrakenConfig {
        retryDelay = Just 300,
        numberOfRetries = 3
       })
