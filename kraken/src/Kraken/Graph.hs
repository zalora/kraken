{-# LANGUAGE DeriveFunctor, GADTs, QuasiQuotes #-}

module Kraken.Graph (
    Target(..),
    Monitor(..),

    Node(..),
    NodeMonitor(..),
    toNode,
    toGraph,

    transitiveReduction,
    transitiveHull,
    reachableVerticesWithoutSelf,
    cycles,
  ) where


import           Data.Graph.Wrapper as Graph
import           Data.List          as List
import           Data.Maybe

import           Kraken.ActionM
import           Kraken.Util


-- | Type representing targets.
data Target = Target {
    name :: TargetName,
    dependencies :: [TargetName],
    action :: TargetM (),
    monitor :: Maybe Monitor
  }

data Monitor where
  Monitor :: {
      monitorName :: TargetName,
      monitorDependencies :: [TargetName],
      monitorAction :: Maybe monitorInput -> MonitorM monitorInput ()
    } -> Monitor


-- | Node type for the target graph, stripped of dependencies.
-- We don't want to store target dependencies redundantly, so this is
-- a stripped down version of Kraken.Target.Target.
data Node = Node {
    nodeAction :: TargetM (),
    nodeMonitor :: Maybe NodeMonitor
  }

data NodeMonitor where
  NodeMonitor :: {
      nodeMonitorName :: TargetName,
      nodeMonitorAction :: Maybe monitorInput -> MonitorM monitorInput ()
    } -> NodeMonitor

toNode :: Target -> Node
toNode (Target _name _deps action monitor) =
  Node action (fmap toNodeMonitor monitor)

toNodeMonitor :: Monitor -> NodeMonitor
toNodeMonitor (Monitor name _deps action) =
  NodeMonitor name action


-- The monitors are being inserted into the Node graph as targets,
-- additionally to being stored as monitors for their targets.
toGraph :: [Target] -> Either String (Graph TargetName Node)
toGraph targets = do
    let monitorDependencies :: Maybe Monitor -> [TargetName]
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
    monitorToNode :: Monitor -> (TargetName, Node, [TargetName])
    monitorToNode (Monitor name deps action) =
        (name, Node (discardMonitorInput $ action Nothing) Nothing, deps)

    checkAcyclic g = case cycles g of
        [] -> Right ()
        is -> Left $ unlines $
            "cycles in target graph:" :
            fmap (unwords . (fmap show)) is

