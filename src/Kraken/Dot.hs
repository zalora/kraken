{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Kraken.Dot (
  DotNode,
  fromNode,
  fromWebNode,
  toDot,
  mapPrefixes,
 ) where


import           Control.Arrow
import           Data.Foldable           as Foldable
import           Data.Function
import           Data.Graph.Wrapper      as Graph
import           Data.List               as List (filter, isPrefixOf, nub,
                                                  sortBy)
import           Data.Maybe
import           Data.String.Interpolate
import           Prelude                 hiding (any, concat, elem)
import           Safe

import           Kraken.ActionM
import qualified Kraken.Graph
import qualified Kraken.Web.TargetGraph as Web
import           Kraken.Util


data DotNode = DotNode {
  monitor :: Maybe TargetName
 }


-- * conversions from other types

fromNode :: Kraken.Graph.Node -> DotNode
fromNode (Kraken.Graph.Target _name _deps _action monitor) =
  DotNode (fmap Kraken.Graph.monitorName monitor)

fromWebNode :: Web.WebNode -> DotNode
fromWebNode (Web.WebNode _name monitor) =
  DotNode monitor


-- * conversion to dot

toDot :: Bool -> Maybe [String] -> Bool -> (Graph TargetName DotNode) -> String
toDot withMonitors prefixes transitiveReductionFlag graph = unlines $
    "digraph targets {" :
    "    rankdir = RL;" :
    fmap ("    " ++) (targetsToEdges withMonitors processedGraph) ++
    "}" :
    []
  where
    processedGraph =
      (if transitiveReductionFlag then transitiveReduction else id) $
      filterByPrefix prefixes graph

filterByPrefix :: Maybe [String] -> Graph TargetName DotNode -> Graph TargetName DotNode
filterByPrefix Nothing = id
filterByPrefix (Just (mapPrefixes -> prefixes)) =
    Graph.toList >>>
    -- filter out nodes
    List.filter (hasPrefix . fst3) >>>
    -- filter out deps by prefix (including monitors)
    fmap (\ (name, DotNode monitor, deps) ->
      (dropPrefix name,
       DotNode (maybe Nothing dropPrefixesFromMonitor monitor),
       (fmap dropPrefix $ List.filter hasPrefix deps))) >>>
    Graph.fromList

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
    dropPrefixesFromMonitor :: TargetName -> Maybe TargetName
    dropPrefixesFromMonitor monitorName = if hasPrefix monitorName
        then Just (dropPrefix monitorName)
        else Nothing

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


targetsToEdges :: Bool -> Graph TargetName DotNode -> [String]
targetsToEdges withMonitors graph =
    concat $ for targets $ \ target ->
        targetEdges withMonitors graph (target, vertex graph target)
  where
    targets :: [TargetName]
    targets =
        List.filter (\ t -> not (t `elem` monitors)) $
        reverse $ topologicalSort graph
    monitors :: [TargetName]
    monitors = catMaybes $ fmap monitor $ Foldable.toList graph

targetEdges :: Bool -> Graph TargetName DotNode -> (TargetName, DotNode) -> [String]
targetEdges withMonitors graph (name, node) =
    [i|"#{name}" [shape = #{nodeShape}];|] :
    (fmap (mkEdge "black" name) dependencies) ++
    if withMonitors then
        maybe [] (\ monitorName ->
            [i|"#{monitorName}" [color = "blue"];|] :
            (mkEdge "blue" name monitorName ++ " // monitor_edge") : [])
            (monitor node)
      else []
  where
    nodeShape = maybe "oval" (const "box") (monitor node)
    dependencies = successors graph name

mkEdge :: String -> TargetName -> TargetName -> String
mkEdge color a b =
    [i|"#{a}" -> "#{b}" [color = "#{color}"];|]


-- * utils

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
