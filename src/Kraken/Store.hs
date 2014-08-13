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
import           Data.List               (foldl', group, isPrefixOf, nub, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Set                as Set (Set, empty, fromList, insert,
                                                 isSubsetOf, member, (\\))
import           Data.String.Interpolate
import           Data.Traversable        (forM)
import           Options.Applicative
import           System.Exit
import           System.IO

import           Kraken.Graph
import           Kraken.Target
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
        if monitors `isSubsetOf` targetNames then
            Right ()
        else
            Left ("monitors cannot be found: " ++
                (unwords $ sort $ fmap show $ toList (monitors \\ targetNames)))
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
        fmap fromMonitor $ catMaybes $
        fmap monitor targets
    allDependencies :: Set TargetName
    allDependencies = Set.fromList $ concat $
        fmap dependencies targets


data TargetList
    = AllTargets -- meaning all available targets
    | SelectedTargets [TargetName]
  deriving (Show)

lookupTargets :: Store -> Bool -> TargetList -> TargetM [TargetName]
lookupTargets store _ AllTargets = return $ map nodeName $ toList $ graph store
lookupTargets store useAsPrefix (SelectedTargets names) =
    map nodeName <$>
    concat <$>
    (forM names $ \ needle ->
     case filter (pred needle) (toList $ graph store) of
        [target] -> return [target]
        [] -> abort [i|target not found: #{needle}|]
        targets -> if useAsPrefix
            then return targets
            else abort [i|multiple targets found for prefix #{needle}|])
  where
    -- whether to include a given Target
    pred :: TargetName -> Node -> Bool
    pred needle t = if useAsPrefix
        then show needle `isPrefixOf` show (nodeName t)
        else nodeName t == needle

lookupTarget :: Store -> TargetName -> TargetM Node
lookupTarget store targetName =
    case filter (\ t -> nodeName t == targetName) (toList $ graph store) of
        [a] -> return a
        _ -> abort [i|unable to look up target: #{targetName}|]

-- | returns all targets to be executed for running a given target, i.e.
-- including dependencies and the given target itself.
lookupExecutionPlan :: Store -> Bool -> [TargetName] -> [TargetName]
lookupExecutionPlan _ _dontChaseDependencies@True targets = targets
lookupExecutionPlan store _dontChaseDependencies@False targets = do
    foldDependencies ((++), []) store targets (\ node -> [nodeName node])

lookupDependencies :: Store -> TargetName -> [TargetName]
lookupDependencies store target =
    filter (/= target) $
    foldDependencies ((++), []) store [target] (\ node -> [nodeName node])

-- The first argument is morally a Monoid instance constraint. But for some of the
-- contexts this is used in, there would be multiple possible Monoid instances
-- (e.g. for TargetM). Therefore this function requires the first argument
-- explicitly to make it a bit clearer what's going on.
foldDependencies :: (a -> a -> a, a) -> Store -> [TargetName] -> (Node -> a) -> a
foldDependencies monoid@(_, mempty) store targets f =
  foldTopologically monoid store $ \ node ->
    if nodeName node `Set.member` reachable
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
    List _ -> putStr $ unlines $
        fmap show $
        reverse $ topologicalSort $
        graph store
    Dot _ withMonitors prefixes transitiveReduction ->
        putStr $ toDot withMonitors prefixes transitiveReduction $ originalTargets store
  where
    reportAndExit :: [(Maybe TargetName, String)] -> IO ()
    reportAndExit messages = do
        hPutStr stderr $ unlines $
            "" :
            "FAILURE" :
            "-------" :
            (strip $ unlines $ map showFailure messages) :
            []
        exitWith (ExitFailure 70)


-- * running

-- | Will run all given targets, and collect their error messages.
runTargets :: Store -> Bool -> Bool -> Bool -> Bool -> [TargetName] -> TargetM ()
runTargets store dryRun dontChaseDependencies omitMonitors failFast targets = do
    let executionPlan = lookupExecutionPlan store dontChaseDependencies targets
    logMessage . unlines $
        "execution plan:" :
        (fmap (("    " ++) . show) executionPlan)
    when (not dryRun) $ do
        doneTargets <- liftIO $ newMVar Set.empty
        foldDependencies ((>>), return ()) store targets $ \ node -> do
            done <- liftIO $ readMVar doneTargets
            let dependencies = lookupDependencies store (nodeName node)
            when (all (`Set.member` done) dependencies) $ do
                isolateM $ do
                    runTargetWithMonitor store omitMonitors node
                    liftIO $ modifyMVar_ doneTargets (return . insert (nodeName node))
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
runTargetWithMonitor :: Store -> Bool -> Node -> TargetM ()
runTargetWithMonitor _ _omitMonitors target@Node{nodeMonitor = Nothing} =
    runTarget target
runTargetWithMonitor _ True target =
    runTarget target
runTargetWithMonitor store False target@Node{nodeMonitor = Just (Monitor monitor)} =
  withTargetName (nodeName target) $ do
    logMessageLn [i|running monitor for #{nodeName target}|]
    monitorTarget <- lookupTarget store monitor
    monitorMessages <- liftIO $ runTargetMSilently $ runTarget monitorTarget
    case monitorMessages of
        Right () -> do
            logMessageLn [i|monitor returned no messages, not running #{nodeName target}|]
        Left messages -> do
            logMessageLn "monitor returned messages:"
            forM_ messages $ \ (mName, message) ->
                logMessageLn $ case mName of
                    Nothing -> message
                    Just name -> show name ++ ": " ++ message
            -- running the actual target (inside the TargetM monad)
            runTarget target
            -- running the monitor as confirmation (inside the TargetM monad)
            runTarget monitorTarget
            logMessageLn [i|monitor ran successfully: #{nodeName monitorTarget}|]

-- | Runs the target ignoring dependencies and monitors.
runTarget :: Node -> TargetM ()
runTarget target = withTargetName (nodeName target) $ do
    logMessageLn [i|running target #{nodeName target}|]
    nodeRun target


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
    prefixes = (\ ps -> if null ps then Nothing else Just ps) <$> many (strOption (
        long "prefix" <>
        short 'p' <>
        help "only include targets with the given prefix (and strip the prefix)"))

    transitiveReduction :: Parser Bool
    transitiveReduction = switch (
        long "transitive-reduction" <>
        short 't' <>
        help "output the transitive reduction of the graph")
