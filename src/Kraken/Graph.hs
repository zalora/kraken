{-# LANGUAGE DeriveFunctor, ExistentialQuantification, GADTs, QuasiQuotes #-}

module Kraken.Graph (
    TargetPoly(..),
    Target,
    MonitorPoly(..),
    Monitor,

    Node,
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
data TargetPoly dependencies = Target {
    name :: TargetName,
    dependencies :: dependencies,
    action :: TargetM (),
    monitor :: Maybe (MonitorPoly dependencies)
  }
    deriving (Functor)

-- | Target type that still contains its dependencies. Users of kraken use this
-- type to specify their targets.
type Target = TargetPoly [TargetName]


data MonitorPoly dependencies where
  Monitor :: {
      monitorName :: TargetName,
      monitorDependencies :: dependencies,
      monitorAction :: (Maybe monitorInput -> MonitorM monitorInput ())
    } -> MonitorPoly dependencies

type Monitor = MonitorPoly [TargetName]

instance Functor MonitorPoly where
    fmap f (Monitor name deps action) = Monitor name (f deps) action


-- | Node type for the target graph, stripped of dependencies.
-- We don't want to store target dependencies redundantly, so this is
-- a stripped down version of Kraken.Target.Target.
-- Also the monitors are being inserted into the Node graph as targets,
-- additionally to being stored as monitors for other targets.
type Node = TargetPoly ()

toNode :: Target -> Node
toNode = fmap (const ())


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
        (name, Target name () (discardMonitorInput $ action Nothing) Nothing, deps)

    checkAcyclic g = case cycles g of
        [] -> Right ()
        is -> Left $ unlines $
            "cycles in target graph:" :
            fmap (unwords . (fmap show)) is
