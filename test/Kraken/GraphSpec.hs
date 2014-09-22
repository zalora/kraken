{-# LANGUAGE FlexibleInstances, OverloadedStrings, QuasiQuotes,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kraken.GraphSpec (main, spec) where


import           Control.Applicative
import           Data.Graph.Wrapper      as Graph
import           Data.List
import           Data.String.Interpolate
import           Data.Traversable
import           System.Process
import           Test.Hspec
import           Test.QuickCheck

import           Kraken.Dot
import           Kraken.Graph


instance Arbitrary (Graph Int ()) where
    arbitrary = resize 10 $ do
        nodes <- nub <$> arbitrary
        Graph.fromList <$> forM nodes (\ node -> do
            successors <- nub <$> listOf (elements nodes)
            return (node, (), successors))
    shrink = fmap Graph.fromListLenient . shrink . Graph.toList

acyclicGraph :: Gen (Graph Int ())
acyclicGraph =
    fromListLenient <$>
    fmap (\ (i, (), successors) -> (i, (), filter (< i) successors)) <$>
    toList <$>
    arbitrary

-- | Generates an arbitrary node that is in the graph.
arbitraryNode :: (Show i, Eq i, Testable prop) =>
    Graph i v -> (i -> prop) -> Property
arbitraryNode g p =
    (not $ null $ vertices g) ==>
    forAll (elements $ vertices g) $ \ i ->
    (i `elem` vertices g) ==>
    property $
    p i

-- | Generates an arbitrary edge that is in the graph.
arbitraryEdge :: (Show i, Ord i, Testable prop) =>
    Graph i v -> ((i, i) -> prop) -> Property
arbitraryEdge g p =
    (not $ null $ edges g) ==>
    forAll gen $ \ e ->
    (e `elem` edges g) ==>
    property $
    p e
  where
    gen = do
        a <- elements $ filter (not . null . successors g) $ vertices g
        b <- elements (successors g a)
        return (a, b)


-- * graph utils

-- | Returns whether the given node is in an SCC.
inCyclicSCC :: Eq i => Graph i v -> i -> Bool
inCyclicSCC g i = i `elem` csccs (stronglyConnectedComponents g)
  where
    csccs (AcyclicSCC _ : r) = csccs r
    csccs (CyclicSCC is : r) = is ++ csccs r
    csccs [] = []

equals :: (Eq i, Ord i, Eq v, Ord v) => Graph i v -> Graph i v -> Bool
equals a b = sort (toList a) == sort (toList b)

removeEdge :: Ord i => (i, i) -> Graph i v -> Graph i v
removeEdge (a, b) = Graph.fromList . inner . Graph.toList
  where
    inner = fmap $ \ (i, v, successors) ->
        if i == a then (i, v, filter (/= b) successors) else (i, v, successors)


-- * entry functions

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "reachableVerticesWithoutSelf" $ do
        it "bla" $ do
            reachableVerticesWithoutSelf (Graph.fromList [(0, (), [])] :: Graph Int ()) 0
                `shouldBe` []
            reachableVerticesWithoutSelf (Graph.fromList [(0, (), [0])] :: Graph Int ()) 0
                `shouldBe` [0]
        it "does not list the node itself by default" $
            -- using acyclic graphs because otherwise quickcheck gives up
            -- too early
            forAll acyclicGraph $ \ (g :: Graph Int ()) ->
            arbitraryNode g $ \ i ->
                not (inCyclicSCC g i) ==>
                    not (i `elem` reachableVerticesWithoutSelf g i)

    describe "transitiveHull" $ do
        it "creates the transitive hull" $
            property $ \ (g :: Graph Int ()) ->
            arbitraryNode g $ \ i ->
                sort (successors (transitiveHull g) i) ==
                sort (reachableVerticesWithoutSelf g i)

    describe "transitiveReduction" $ do
        it "has the same reachability relations as the input graph" $
            forAll acyclicGraph $ \ (g :: Graph Int ()) ->
            null (cycles g) ==>
            whenFail (dumpDotFiles $
                ("input", g) :
                ("hull", transitiveHull g) :
                ("red", transitiveReduction g) :
                []) $
            arbitraryNode g $ \ i ->
                reachableVerticesWithoutSelf g i `shouldMatchList`
                reachableVerticesWithoutSelf (transitiveReduction g) i

        it "is minimal" $
            forAll acyclicGraph $ \ (g :: Graph Int ()) ->
            null (cycles g) ==>
            whenFail (dumpDotFiles $
                ("input", g) :
                ("hull", transitiveHull g) :
                ("red", transitiveReduction g) :
                []) $
            let reduction = transitiveReduction g
            in arbitraryEdge reduction $ \ e ->
                not $ equals
                    (transitiveHull (removeEdge e reduction))
                    (transitiveHull reduction)

    describe "mapPrefixes" $ do
        it "always returns unique abbreviations" $ do
            property $ \ prefixes ->
                unique (fmap snd (mapPrefixes prefixes))

        it "always returns abbreviations that end in a dot" $ do
            property $ \ prefixes ->
                counterexample (show $ mapPrefixes prefixes) $
                all (\ p -> p == "" || last p == '.')
                  (fmap snd $ mapPrefixes prefixes)

    describe "toGraph" $ do
        it "includes the monitors in the store as normal targets" $ do
            let Right g = toGraph $
                    Target "target" [] (return ()) (Just (Monitor "monitor" [] (const (return ())))) :
                    []
            vertices g `shouldContain` ["monitor"]


unique :: (Show a, Eq a) => [a] -> Property
unique list = case filter (\ e -> length (filter (== e) list) > 1) list of
  [] -> property True
  doubles ->
    counterexample (show list ++ " containes these elements more than once: " ++ show doubles) $
    False


dumpDotFiles :: [(String, Graph Int ())] -> IO ()
dumpDotFiles = mapM_ (uncurry dumpDotFile)

dumpDotFile :: String -> Graph Int () -> IO ()
dumpDotFile s g = do
    writeFile [i|#{s}.dot|] (dot g)
    _ <- system [i|cat #{s}.dot | dot -Tpdf > #{s}.pdf|]
    return ()

dot :: Graph Int () -> String
dot g = unlines $
    "digraph g {" :
    fmap (\ i -> "    " ++ show i ++ ";") (vertices g) ++
    fmap (\ (a, b) -> "    " ++ show a ++ " -> " ++ show b ++ ";") (edges g) ++
    "}" :
    []
