{-# LANGUAGE OverloadedStrings #-}

module Kraken.DotSpec where


import           Control.DeepSeq
import           Test.Hspec

import           Kraken
import           Kraken.Dot


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDot" $ do
    it "correctly handles monitors with prefixes" $ do
      let result = toDot False (Just ["p."]) False $
            Target "p.a" [] (return ()) (Just $ Monitor "p.m" ["p.b"] (const (return ()))) :
            Target "p.b" [] (return ()) Nothing :
            []
      deepseq result () `shouldBe` ()
