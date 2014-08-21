{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Kraken.Store (
    Store(graph),
    createStore,
    runAsMain,
    parseKrakenOptions,
    Options(customOptions),
    runStore,
  ) where


import           Control.Concurrent.MVar
import           Control.Monad           (when)
import           Control.Monad.IO.Class
import           Data.Foldable           (toList)
import           Data.Foldable           (forM_)
import           Data.Graph.Wrapper      as Graph hiding (toList)
import           Data.List               as List (foldl', group, isPrefixOf,
                                                  nub, null, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Set                as Set (Set, empty, fromList, insert,
                                                 intersection, isSubsetOf,
                                                 member, null, (\\))
import           Data.String.Interpolate
import           Data.Traversable        (forM)
import           Options.Applicative hiding (action)
import           System.Exit
import           System.IO

import           Kraken.Dot
import           Kraken.Graph
import           Kraken.TargetM
import           Kraken.Util


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
    targetNames <- checkUniqueTargetNames targets
    checkMonitors targetNames
    checkDependencies targetNames
    graph <- toGraph targets
    return $ Store graph targets
  where
    checkMonitors targetNames =
        if Set.null (intersection monitors targetNames)
            then Right()
            else Left ("Please, specify monitors only once inside the target:: " ++
                (unwords $ sort $ fmap show $ toList (intersection monitors targetNames)))
    checkDependencies targetNames =
        if allDependencies `isSubsetOf` targetNames then
            Right ()
        else
            Left ("target dependencies cannot be found: " ++
                 (unwords $ fmap show $ toList (allDependencies \\ targetNames)))

    checkUniqueTargetNames targets =
        case filter (\ g -> length g > 1) (group (sort (fmap name targets))) of
            [] -> Right $ Set.fromList $ fmap name targets
            doubles -> Left ("doubled target names: " ++
                unwords (fmap show $ nub $ concat doubles))

    monitors :: Set TargetName
    monitors = Set.fromList $
        fmap monitorName $ catMaybes $
        fmap monitor targets
    allDependencies :: Set TargetName
    allDependencies = Set.fromList $ concat $
        fmap dependencies targets

evalStore :: Store -> IO ()
evalStore (Store _ _) = return ()


data TargetList
    = AllTargets -- meaning all available targets
    | SelectedTargets [TargetName]
  deriving (Show)

lookupTargets :: Store -> Bool -> TargetList -> TargetM () [TargetName]
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

lookupTarget :: Store -> TargetName -> TargetM () Node
lookupTarget store targetName =
    case filter (\ t -> name t == targetName) (toList $ graph store) of
        [a] -> return a
        _ -> cancel [i|unable to look up target: #{targetName}|]

-- | returns all targets to be executed for running a given target, i.e.
-- including dependencies and the given target itself.
lookupExecutionPlan :: Store -> Bool -> [TargetName] -> TargetM () [Node]
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


-- | Will run a process exposing a command line interface allowing
-- to do diffirent things with the given store:
--
-- - list available targets
-- - run creation/monitoring operations for targets
-- - run as a daemon to regularly execute creation operations
runAsMain :: String -> Store -> IO ()
runAsMain description store = do
    options :: Options () <- parseKrakenOptions description (pure ())
    runStore store options

parseKrakenOptions :: String -> Parser custom -> IO (Options custom)
parseKrakenOptions description customParser = execParser (options description customParser)

runStore :: Store -> Options a -> IO ()
runStore store opts = case opts of
    Run _ targetList dryRun useAsPrefix dontChaseDependencies omitMonitors failFast -> do
        result <- runTargetM $ do
            targets <- lookupTargets store useAsPrefix targetList
            runTargets store dryRun dontChaseDependencies omitMonitors failFast targets
        either reportAndExit return result
    Check _ -> do
        -- Checks are already performed by 'createStore'.
        evalStore store
        logMessageLn "Store is consistent."
    List _ -> putStr $ unlines $
        fmap show $
        reverse $ topologicalSort $
        graph store
    Dot _ withMonitors prefixes transitiveReduction ->
        putStr $ toDot withMonitors prefixes transitiveReduction $ originalTargets store
  where
    reportAndExit :: [Error] -> IO ()
    reportAndExit messages = do
        hPutStr stderr $ unlines $
            "" :
            "FAILURE" :
            "-------" :
            (strip $ unlines $ map showError messages) :
            []
        exitWith (ExitFailure 70)


-- * running

-- | Will run all given targets, and collect their error messages.
runTargets :: Store -> Bool -> Bool -> Bool -> Bool -> [TargetName] -> TargetM () ()
runTargets store dryRun dontChaseDependencies omitMonitors failFast targets = do
    executionPlan <- lookupExecutionPlan store dontChaseDependencies targets
    logMessage . unlines $
        "execution plan:" :
        (fmap (("    " ++) . show . name) executionPlan)
    when (not dryRun) $ do
        doneTargets <- liftIO $ newMVar Set.empty
        forM_ executionPlan $ \ node -> do
            done <- liftIO $ readMVar doneTargets
            let dependencies = lookupDependencies store (name node)
            when (all (`Set.member` done) dependencies || dontChaseDependencies) $ do
                isolateM $ do
                    runTargetWithMonitor omitMonitors node
                    liftIO $ modifyMVar_ doneTargets (return . insert (name node))
  where
    isolateM = if failFast then id else isolate

-- | Runs a target including executing the included monitors appropriately,
--   ignoring the included dependencies.
--
-- 1. Run the monitor.
--    If it runs successfully the target already exists, don't do anything else.
--    If it returns messages, assume the target has to be built, disregard the
--    messages and:
-- 2. Run the target creation operation. If any errors occur, stop and report
--    them.
-- 3. Run the monitor again. If any errors occur, stop and report them. If no
--    errors occur than the target was created successfully.
runTargetWithMonitor :: Bool -> Node -> TargetM () ()
runTargetWithMonitor _omitMonitors target@Target{monitor = Nothing} =
    runTarget target
runTargetWithMonitor True target =
    runTarget target
runTargetWithMonitor False target@Target{monitor = Just (Monitor monitorName () monitorAction)} =
  withTargetName (name target) $ do
    logMessageLn [i|running monitor for #{name target}|]

    bracketWithMonitor
        (\ monitorInput -> withTargetName monitorName (monitorAction monitorInput))
        (runTarget target)


-- | Runs the target ignoring dependencies and monitors.
runTarget :: Node -> TargetM () ()
runTarget target = withTargetName (name target) $ do
    logMessageLn [i|running target #{name target}|]
    action target


-- * cli options

data Options custom
    = Run {
        customOptions :: custom,
        _optTargets :: TargetList,
        _dryRun :: Bool,
        _useAsPrefix :: Bool,
        _dontChaseDependencies :: Bool,
        _omitMonitors :: Bool,
        _failFast :: Bool
      }
    | Check {
        customOptions :: custom
      }
    | List {
        customOptions :: custom
      }
    | Dot {
        customOptions :: custom,
        _withMonitors :: Bool,
        _prefixes :: Maybe [String],
        _transitiveReduction :: Bool
      }
  deriving Show

options :: String -> Parser custom -> ParserInfo (Options custom)
options description customParser =
    info (helper <*> parser) (fullDesc <> progDesc description)
  where
    parser = hsubparser (
        command "run"
            (info (Run <$>
                        customParser <*>
                        targetList <*>
                        dryRun <*>
                        useAsPrefix <*>
                        dontChaseDependencies <*>
                        omitMonitors <*>
                        failFast)
                  (progDesc "run creation and monitoring operations for the specified targets")) <>
        command "check"
            (info (Check <$> customParser) (progDesc "perform static checks on the target store")) <>
        command "list"
            (info (List <$> customParser) (progDesc "list all targets")) <>
        command "dot"
            (info (Dot <$> customParser <*> withMonitors <*> prefixes <*> transitiveReduction)
                  (progDesc "output target graph in dot format"))
      )

    targetList :: Parser TargetList
    targetList = toTargetList <$>
         some (argument Just $
            metavar "TARGETS/PREFIXES" <>
            help "command line arguments specify targets (or prefixes) to be run (or \"all\")")
    toTargetList :: [String] -> TargetList
    toTargetList list = case list of
        ["all"] -> AllTargets
        [] -> error "please specify targets individually or \"all\""
        targets | "all" `elem` targets -> error "\"all\" shouldn't be specified along with other targets"
            -- TODO: figure out how to report this as a proper parse error.
        targets -> SelectedTargets $ fmap TargetName targets

    dryRun :: Parser Bool
    dryRun = switch (
        long "dry-run" <>
        short 'd' <>
        help "output the execution plan and don't do anything else")

    useAsPrefix :: Parser Bool
    useAsPrefix = switch (
        long "use-as-prefix" <>
        short 'p' <>
        help "treat given arguments as target name prefixes")

    dontChaseDependencies :: Parser Bool
    dontChaseDependencies = switch (
        long "dont-chase-dependencies" <>
        short 'x' <>
        help "don't chase dependencies, just run the given targets")

    omitMonitors :: Parser Bool
    omitMonitors = switch (
        long "omit-monitors" <>
        help "don't run monitors, just run the given targets")

    failFast :: Parser Bool
    failFast = switch (
        long "fail-fast" <>
        help "fail on first failing target")

    withMonitors :: Parser Bool
    withMonitors = switch (
        long "with-monitors" <>
        short 'm' <>
        help "include monitors in output")

    prefixes :: Parser (Maybe [String])
    prefixes = (\ ps -> if List.null ps then Nothing else Just ps) <$> many (strOption (
        long "prefix" <>
        short 'p' <>
        help "only include targets with the given prefix (and strip the prefix)"))

    transitiveReduction :: Parser Bool
    transitiveReduction = switch (
        long "transitive-reduction" <>
        short 't' <>
        help "output the transitive reduction of the graph")
