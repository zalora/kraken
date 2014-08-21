{-# LANGUAGE DeriveFunctor, GADTs, QuasiQuotes, ViewPatterns #-}

module Kraken.Graph (
    TargetP(..),
    Monitor(..),
    monitorName,
    Target,
    Node,
    toNode,
    toGraph,
    toDot,
    mapPrefixes,
    transitiveReduction,
    transitiveHull,
    reachableVerticesWithoutSelf,
    cycles,
  ) where


import           Control.Arrow
import           Control.Exception
import           Data.Foldable           as Foldable (toList)
import           Data.Function
import           Data.Graph.Wrapper      as Graph
import           Data.List               as List
import           Data.Maybe
import           Data.String.Interpolate
import           Safe

import           Kraken.TargetM


-- | Type representing targets.
data TargetP dependencies = Target {
    name :: TargetName,
    dependencies :: dependencies,
    monitor :: Maybe (Monitor dependencies),
    run :: TargetM () ()
  }
    deriving (Functor)

data Monitor dependencies where
    Monitor :: TargetName ->
               dependencies ->
               (Maybe monitorInput -> TargetM monitorInput ()) ->
               Monitor dependencies

instance Functor Monitor where
    fmap f (Monitor name deps action) = Monitor name (f deps) action

monitorName :: Monitor dependencies -> TargetName
monitorName (Monitor name _ _) = name


-- | Target type that still contains its dependencies.
type Target = TargetP [TargetName]

-- | Node type for the target graph, stripped of dependencies.
-- We don't want to store target dependencies redundantly, so this is
-- a stripped down version of Kraken.Target.Target.
-- Also the monitors are being inserted into the Node graph as targets,
-- additionally to being stored as monitors for other targets.
type Node = TargetP ()

toNode :: Target -> Node
toNode = fmap (const ())

toGraph :: [Target] -> Either String (Graph TargetName Node)
toGraph targets = do
    let monitorDependencies :: Maybe (Monitor [TargetName]) -> [TargetName]
        monitorDependencies Nothing = []
        monitorDependencies (Just (Monitor _ dependencies _)) =
            dependencies
        graph :: Graph TargetName Node
        graph = Graph.fromList $ concat $ for targets $ \ target ->
            ((name target, toNode target,
                nub (dependencies target ++ monitorDependencies (monitor target))) :
             maybeToList (fmap monitorToNode $ monitor target))
    checkAcyclic graph
    return graph
  where
    -- | to be able to use the monitor as a normal target
    monitorToNode :: Monitor [TargetName] -> (TargetName, Node, [TargetName])
    monitorToNode (Monitor name deps action) =
        (name, Target name () Nothing (discardMonitorInput $ action Nothing), deps)

    checkAcyclic g = case cycles g of
        [] -> Right ()
        is -> Left $ unlines $
            "cycles in target graph:" :
            fmap (unwords . (fmap show)) is


-- * dot output

toDot :: Bool -> Maybe [String] -> Bool -> [Target] -> String
toDot withMonitors prefixes transitiveReductionFlag targets = unlines $
    "digraph targets {" :
    fmap ("    " ++) (targetsToEdges withMonitors processedGraph) ++
    "}" :
    []
  where
    processedGraph = case toGraph (filterByPrefix prefixes targets) of
        Left _ -> error ("target list given to toDot is invalid")
        Right g -> if transitiveReductionFlag
            then transitiveReduction g
            else g

filterByPrefix :: Maybe [String] -> [Target] -> [Target]
filterByPrefix Nothing = id
filterByPrefix (Just (mapPrefixes -> prefixes)) =
    -- filter out nodes
    List.filter (hasPrefix . name) >>>
    -- filter out deps by prefix (including monitors)
    fmap (\ (Target name deps monitor action) ->
        Target (dropPrefix name)
            (fmap dropPrefix $ List.filter hasPrefix deps)
            (maybe Nothing (\ (Monitor name deps action) ->
                if hasPrefix name then Just (Monitor (dropPrefix name) deps action) else Nothing) monitor)
            action)
  where
    hasPrefix :: TargetName -> Bool
    hasPrefix n = any (\ prefix -> prefix `isPrefixOf` show n) (fmap fst prefixes)
    -- drops the longest matching prefix
    dropPrefix :: TargetName -> TargetName
    dropPrefix (TargetName n) =
        let (matchingPrefix, replacingPrefix) =
                headNote "Kraken.Graph.filterByPrefix: prefix filtering error"
                (dropWhile (\ (prefix, _) -> not (prefix `isPrefixOf` n)) prefixes)
        in TargetName $ (replacingPrefix ++) $ drop (length matchingPrefix) n

-- | Creates a mapping from prefixes to be replaced by abbreviations.
mapPrefixes :: [String] -> [(String, String)]
mapPrefixes [prefix] = [(prefix, "")]
mapPrefixes (nub -> prefixes) =
  fmap (\ p -> (p, mkAbbreviation 1 p)) $
  reverse $ sortBy (compare `on` length) prefixes
 where
  mkAbbreviation n p = case List.filter (== take n p) $ fmap (take n) prefixes of
    [abbreviation] -> abbreviation ++ "."
    _ -> mkAbbreviation (succ n) p


targetsToEdges :: Bool -> Graph TargetName Node -> [String]
targetsToEdges withMonitors graph =
    concat $ for targets $ \ target ->
        targetEdges withMonitors graph (vertex graph target)
  where
    targets :: [TargetName]
    targets =
        List.filter (\ t -> not (t `elem` monitors)) $
        reverse $ topologicalSort graph
    monitors :: [TargetName]
    monitors = fmap monitorName $ catMaybes $ fmap monitor $ Foldable.toList graph

targetEdges :: Bool -> Graph TargetName Node -> Node -> [String]
targetEdges withMonitors graph node =
    [i|"#{name node}" [shape = #{nodeShape}];|] :
    (fmap (mkEdge "black" node) dependencies) ++
    if withMonitors then
        maybe [] (\ (Monitor name _ _) ->
            [i|"#{name}" [color = "blue"];|] :
            (mkEdge "blue" node name ++ " // monitor_edge") : [])
            (monitor node)
      else []
  where
    nodeShape = maybe "oval" (const "box") (monitor node)
    dependencies = successors graph (name node)

mkEdge :: String -> Node -> TargetName -> String
mkEdge color a b =
    [i|"#{name a}" -> "#{b}" [color = "#{color}"];|]


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

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap
