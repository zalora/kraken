{-# LANGUAGE OverloadedStrings #-}

module Kraken.DotSpec where


import           Control.DeepSeq
import           Data.Graph.Wrapper
import           System.Environment
import           System.IO.Silently
import           Test.Hspec
import           Test.Hspec.Expectations.Contrib
import           Test.QuickCheck

import           Kraken.ActionM
import           Kraken.Dot
import           Kraken.Graph
import           Kraken.Run
import           Kraken.Store


main :: IO ()
main = hspec spec

toGraph' :: [Target] -> Graph TargetName DotNode
toGraph' = undefined -- fmap Kraken.Dot.fromNode . either error id . toGraph

spec :: Spec
spec = do
  describe "toDot" $ do
    it "correctly handles monitors with prefixes" $ do
      let result = toDot False (Just ["p."]) False $ toGraph' $
            Target "p.a" [] (return ()) (Just $ Monitor "p.m" ["p.b"] (const (return ()))) :
            Target "p.b" [] (return ()) Nothing :
            []
      deepseq result () `shouldBe` ()

  describe "mapPrefixes" $ do
    it "always returns unique abbreviations" $ do
      property $ \ prefixes ->
        unique (fmap snd (mapPrefixes prefixes))

    it "always returns abbreviations that end in a dot" $ do
      property $ \ prefixes ->
        counterexample (show $ mapPrefixes prefixes) $
          all (\ p -> p == "" || last p == '.')
              (fmap snd $ mapPrefixes prefixes)

  describe "runAsMain used with dot" $ do
    it "allows to use the global --config option" $ do
      withArgs (words "dot --config kraken.conf.example") $
        runAsMain "test program" (createStore [])

    let targets =
          Target "a" [] (return ()) Nothing :
          Target "b" ["a"] (return ()) Nothing :
          Target "c" ["a"] (return ()) Nothing :
          Target "d" ["b", "c"] (return ()) Nothing :
          []
        priorities = ["c", "b"]
        store = createStoreWithPriorities priorities targets

    it "doesn't show priority dependencies in the graph" $ do
      output <- capture_ $ withArgs ["dot"] $
        runAsMain "test program" store
      output `shouldNotContain` "\"c\" -> \"b\""
      output `shouldNotContain` "\"b\" -> \"c\""

    it "does include ordering numbers" $ do
      output <- capture_ $ withArgs ["dot"] $
        runAsMain "test program" store
      mapM_ (output `shouldContain`) $
        "1. a" :
        "2. c" :
        "3. b" :
        "4. d" :
        []

unique :: (Show a, Eq a) => [a] -> Property
unique list = case filter (\ e -> length (filter (== e) list) > 1) list of
  [] -> property True
  doubles ->
    counterexample (show list ++ " containes these elements more than once: " ++ show doubles) $
    False
