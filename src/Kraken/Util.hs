module Kraken.Util where


import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Graph.Wrapper     as Graph
import           Data.List              as List
import           Data.Maybe
import           System.IO


strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace


logMessage :: MonadIO m => String -> m ()
logMessage = liftIO . hPutStr stderr

logMessageLn :: MonadIO m => String -> m ()
logMessageLn = liftIO . hPutStrLn stderr

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap


-- * graph helpers

cycles :: Graph i v -> [[i]]
cycles g = catMaybes $ fmap cyclicSCCs $ stronglyConnectedComponents g
  where
    cyclicSCCs (AcyclicSCC _) = Nothing
    cyclicSCCs (CyclicSCC is) = Just is

-- | PRE: graph is acyclic
transitiveReduction :: Ord i => Graph i v -> Graph i v
transitiveReduction g =
    assert (List.null (cycles g)) $
    Graph.fromList $ fmap inner $ Graph.toList hull
  where
    inner (i, v, outgoing) =
        (i, v, nub $ List.filter (\ o -> not $ hasIndirectPath i o) outgoing)
    hull = transitiveHull g
    hasIndirectPath a b =
        b `elem`
        (concat $ fmap (reachableVerticesWithoutSelf hull) $ successors hull a)

transitiveHull :: Ord i => Graph i v -> Graph i v
transitiveHull g =
    Graph.fromList $ fmap inner $ Graph.toList g
  where
    inner (i, v, _outgoing) =
        (i, v, reachableVerticesWithoutSelf g i)

-- | Returns all the reachable vertices from one vertex without actually
-- including the node itself if the node is not part of a cycle.
reachableVerticesWithoutSelf :: Ord i => Graph i v -> i -> [i]
reachableVerticesWithoutSelf g i =
    nub $
    concat $ fmap (reachableVertices g) $ successors g i
