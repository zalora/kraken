{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kraken.Store where


import           Data.Foldable           as Foldable (toList)
import           Data.Graph.Wrapper      as Graph
import           Data.List               as List (foldl', group, isPrefixOf,
                                                  nub, sort)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Set                as Set (Set, fromList, isSubsetOf,
                                                 member, union, (\\))
import           Data.String.Interpolate
import           Data.Traversable        (forM)
import           Options.Applicative     hiding (action)

import           Kraken.ActionM
import           Kraken.Graph


data Store = Store {
    -- the graph does not only store the direct given dependencies
    -- but also the dependencies from the given monitor for each target.
    graphWithPriorities :: Graph TargetName Node,
    graphWithoutPriorities :: Graph TargetName Node
  }

createStore :: [Target] -> Store
createStore targets = createStoreWithPriorities [] targets

-- | smart constructor for Store
createStoreWithPriorities :: [TargetName] -> [Target] -> Store
createStoreWithPriorities priorities targets =
    case checkStore (addPriorities priorities targets) of
        Right withPriorities -> case checkStore targets of
          Right withoutPriorities -> Store withPriorities withoutPriorities
          Left err -> error err
        Left err -> error err

addPriorities :: [TargetName] -> [Target] -> [Target]
addPriorities priorities =
    map $ \ target -> case Map.lookup (name target) priorityMap of
      Nothing -> target
      Just prioOutgoing -> target{dependencies = prioOutgoing : dependencies target}
  where
    priorityMap :: Map.Map TargetName TargetName
    priorityMap = case priorities of
      [] -> Map.empty
      all@(_ : r) -> Map.fromList $ zip r all

checkStore :: [Target] -> Either String (Graph TargetName Node)
checkStore targets = do
    checkUniqueTargetNames
    checkDependencies
    toGraph targets
  where
    checkUniqueTargetNames =
        case filter (\ g -> length g > 1) (group (sort (targetNames ++ monitorNames))) of
            [] -> Right ()
            doubles -> Left ("doubled target names: " ++
                unwords (fmap show $ nub $ concat doubles))

    checkDependencies =
        if allDependencies `isSubsetOf` targetAndMonitorNames then
            Right ()
        else
            Left ("target dependencies cannot be found: " ++
                 (unwords $ fmap show $ sort $ Foldable.toList (allDependencies \\ targetAndMonitorNames)))

    monitorNames :: [TargetName]
    monitorNames = fmap monitorName $ catMaybes $ fmap monitor targets

    targetNames :: [TargetName]
    targetNames = fmap name targets

    targetAndMonitorNames :: Set TargetName
    targetAndMonitorNames = Set.fromList (targetNames ++ monitorNames)

    allDependencies :: Set TargetName
    allDependencies =
        Set.fromList (concat (map dependencies targets)) `union`
        Set.fromList (concat (map monitorDependencies (catMaybes (map monitor targets))))

evalStore :: Store -> IO ()
evalStore (Store _ _) = return ()


data TargetList
    = AllTargets -- meaning all available targets
    | SelectedTargets [TargetName]
  deriving (Show)

lookupTargets :: Store -> Bool -> TargetList -> TargetM [TargetName]
lookupTargets store _ AllTargets = return $ vertices $ graphWithPriorities store
lookupTargets store useAsPrefix (SelectedTargets names) =
    concat <$>
    (forM names $ \ needle ->
     case filter (pred needle) (vertices $ graphWithPriorities store) of
        [target] -> return [target]
        [] -> cancel [i|target not found: #{needle}|]
        targets -> if useAsPrefix
            then return targets
            else cancel [i|multiple targets found for prefix #{needle}|])
  where
    -- whether to include a given Target
    pred :: TargetName -> TargetName -> Bool
    pred needle target = if useAsPrefix
        then show needle `isPrefixOf` show target
        else target == needle

lookupTarget :: Store -> TargetName -> TargetM (TargetName, Node)
lookupTarget store targetName =
    case filter (\ (n, _, _) -> n == targetName) (Graph.toList $ graphWithPriorities store) of
        [(name, node, _)] -> return (name, node)
        _ -> cancel [i|unable to look up target: #{targetName}|]

-- | returns all targets to be executed for running a given target, i.e.
-- including dependencies and the given target itself.
lookupExecutionPlan :: Store -> Bool -> [TargetName] -> TargetM [(TargetName, Node)]
lookupExecutionPlan store _dontChaseDependencies@True targets =
    mapM (lookupTarget store) targets
lookupExecutionPlan store _dontChaseDependencies@False targets = do
    mapM_ (lookupTarget store) targets
    let plan graph = foldDependencies ((++), []) graph targets (\ (name, node) -> [(name, node)])
        with = plan (graphWithPriorities store)
        without = map fst $ plan (graphWithoutPriorities store)
    return $ filter ((`elem` without) . fst) with

lookupDependencies :: Store -> TargetName -> [TargetName]
lookupDependencies store target =
    filter (/= target) $
    foldDependencies ((++), []) (graphWithoutPriorities store) [target] (\ (name, _) -> [name])


-- The first argument is morally a Monoid instance constraint. But for some of the
-- contexts this is used in, there would be multiple possible Monoid instances
-- (e.g. for TargetM). Therefore this function requires the first argument
-- explicitly to make it a bit clearer what's going on.
foldDependencies :: (a -> a -> a, a) -> Graph TargetName Node -> [TargetName] -> ((TargetName, Node) -> a) -> a
foldDependencies monoid@(_, mempty) graph targets f =
  foldTopologically monoid graph $ \ (name, node) ->
    if name `Set.member` reachable
      then f (name, node)
      else mempty
 where
  reachable :: Set TargetName
  reachable = Set.fromList $ concatMap (reachableVertices graph) targets

foldTopologically :: forall a . (a -> a -> a, a) -> Graph TargetName Node -> ((TargetName, Node) -> a) -> a
foldTopologically ((<>), mempty) graph f =
    foldl' inner mempty (reverse $ topologicalSort graph)
  where
    inner :: a -> TargetName -> a
    inner acc target = acc <> f (target, vertex graph target)
