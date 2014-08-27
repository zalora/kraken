{-# LANGUAGE OverloadedStrings #-}

module Kraken.StoreSpec where


import           Test.Hspec

import           Kraken.Graph
import           Kraken.Store


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "checkStore" $ do
        it "complains about monitors with non-existing dependencies" $ do
          let result = checkStore $
                  Target "t" [] (return ()) (Just (Monitor "m" ["foo"] (const (return ())))) :
                  []
          fmap (const ()) result `shouldBe` Left "target dependencies cannot be found: foo"
