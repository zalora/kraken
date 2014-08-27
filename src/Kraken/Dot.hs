{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Kraken.Dot (toDot, mapPrefixes) where


import           Control.Arrow
import           Data.Foldable           as Foldable
import           Data.Function
import           Data.Graph.Wrapper
import           Data.List               as List (filter, isPrefixOf, nub,
                                                  sortBy)
import           Data.Maybe
import           Data.String.Interpolate
import           Prelude                 hiding (any, concat, elem)
import           Safe

import           Kraken.ActionM
import           Kraken.Graph
import           Kraken.Util


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
    fmap (\ (Target name deps action monitor) ->
        Target (dropPrefix name)
            (fmap dropPrefix $ List.filter hasPrefix deps)
            action
            (maybe Nothing dropPrefixesFromMonitor monitor))

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
    dropPrefixesFromMonitor :: Monitor -> Maybe Monitor
    dropPrefixesFromMonitor (Monitor name deps action) = if hasPrefix name
        then Just (Monitor (dropPrefix name) (map dropPrefix $ List.filter hasPrefix deps) action)
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
