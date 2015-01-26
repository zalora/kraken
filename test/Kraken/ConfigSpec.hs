

module Kraken.ConfigSpec where


import           Test.Hspec

import           Kraken.Config


spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads the example config" $ do
      (loadKrakenConfig "kraken.conf.example") `shouldReturn` (KrakenConfig {
        retryDelay = Just 300,
        numberOfRetries = 3
       })
