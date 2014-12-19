{-# LANGUAGE ScopedTypeVariables #-}

-- | This module just exists to compile example/Example.hs during compilation
-- of the test suite.

module ExampleSpec where


import           System.Environment
import           Test.Hspec

import           Example


spec :: Spec
spec = do
    it "executing the example file" $ do
        withArgs ["check"] Example.main
