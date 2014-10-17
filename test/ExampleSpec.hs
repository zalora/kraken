{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}

-- | This module just exists to compile example/Example.hs during compilation
-- of the test suite.

module ExampleSpec where


import           Data.Aeson
import           Network.Wai.Test
import           System.Environment
import           Test.Hspec
import           Test.Hspec.Wai

import           Example
import           RestExample


spec :: Spec
spec = do
  describe "Example" $ do
    it "executing the example file" $ do
      withArgs ["check"] Example.main

  with (return application) $ do
    describe "RestExample" $ do
      context "/bar" $ do
        it "serves an Animal" $ do
          response <- get "/bar"
          return response `shouldRespondWith` 200
          liftIO $ do
            let expected = Animal "Jerry" "mouse"
            decode' (simpleBody response) `shouldBe` Just expected
