{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Kraken.Store where


import           Data.Foldable           (toList)
import           Data.Graph.Wrapper      as Graph hiding (toList)
import           Data.List               as List (foldl', group, isPrefixOf,
                                                  nub, sort)
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
    graph :: Graph TargetName Node,
    -- the original targets (needed by Kraken.Graph.toDot)
    originalTargets :: [Target]
  }

-- | smart constructor for Store
createStore :: [Target] -> Store
createStore targets =
    case checkStore targets of
        Right store -> store
        Left err -> error err

checkStore :: [Target] -> Either String Store
checkStore targets = do
    checkUniqueTargetNames
    checkDependencies
    graph <- toGraph targets
    return $ Store graph targets
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
                 (unwords $ fmap show $ sort $ toList (allDependencies \\ targetAndMonitorNames)))

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
lookupTargets store _ AllTargets = return $ map name $ toList $ graph store
lookupTargets store useAsPrefix (SelectedTargets names) =
    map name <$>
    concat <$>
    (forM names $ \ needle ->
     case filter (pred needle) (toList $ graph store) of
        [target] -> return [target]
        [] -> cancel [i|target not found: #{needle}|]
        targets -> if useAsPrefix
            then return targets
            else cancel [i|multiple targets found for prefix #{needle}|])
  where
    -- whether to include a given Target
    pred :: TargetName -> Node -> Bool
    pred needle t = if useAsPrefix
        then show needle `isPrefixOf` show (name t)
        else name t == needle

lookupTarget :: Store -> TargetName -> TargetM Node
lookupTarget store targetName =
    case filter (\ t -> name t == targetName) (toList $ graph store) of
        [a] -> return a
        _ -> cancel [i|unable to look up target: #{targetName}|]

-- | returns all targets to be executed for running a given target, i.e.
-- including dependencies and the given target itself.
lookupExecutionPlan :: Store -> Bool -> [TargetName] -> TargetM [Node]
lookupExecutionPlan store _dontChaseDependencies@True targets =
    mapM (lookupTarget store) targets
lookupExecutionPlan store _dontChaseDependencies@False targets = do
    mapM_ (lookupTarget store) targets
    return $ foldDependencies ((++), []) store targets (\ node -> [node])

lookupDependencies :: Store -> TargetName -> [TargetName]
lookupDependencies store target =
    filter (/= target) $
    foldDependencies ((++), []) store [target] (\ node -> [name node])

-- The first argument is morally a Monoid instance constraint. But for some of the
-- contexts this is used in, there would be multiple possible Monoid instances
-- (e.g. for TargetM). Therefore this function requires the first argument
-- explicitly to make it a bit clearer what's going on.
foldDependencies :: (a -> a -> a, a) -> Store -> [TargetName] -> (Node -> a) -> a
foldDependencies monoid@(_, mempty) store targets f =
  foldTopologically monoid store $ \ node ->
    if name node `Set.member` reachable
      then f node
      else mempty
 where
  reachable :: Set TargetName
  reachable = Set.fromList $ concatMap (reachableVertices (graph store)) targets

foldTopologically :: forall a . (a -> a -> a, a) -> Store -> (Node -> a) -> a
foldTopologically ((<>), mempty) store f =
    foldl' inner mempty (reverse $ topologicalSort $ graph store)
  where
    inner :: a -> TargetName -> a
    inner acc target = acc <> f (vertex (graph store) target)
