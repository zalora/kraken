{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Kraken.Run (
  runAsMain,
  parseKrakenOptions,
  Options(customOptions),
  runStore,
 ) where


import           Control.Concurrent.MVar
import           Control.Monad            (when)
import           Control.Monad.IO.Class
import           Data.Foldable            (forM_)
import           Data.Graph.Wrapper       as Graph hiding (toList)
import           Data.List                as List (null)
import           Data.Monoid
import           Data.Set                 as Set (empty, insert, member)
import           Data.String.Interpolate
import           Network.Wai.Handler.Warp hiding (cancel)
import           Options.Applicative      hiding (action)
import           Safe
import           System.Exit
import           System.IO

import           Kraken.ActionM
import           Kraken.Daemon
import           Kraken.Dot
import           Kraken.Graph
import           Kraken.Store
import           Kraken.Util


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
        result <- runActionM $ do
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
    Daemon _ port -> run port (daemon store)
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
runTargets :: Store -> Bool -> Bool -> Bool -> Bool -> [TargetName] -> TargetM ()
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
runTargetWithMonitor :: Bool -> Node -> TargetM ()
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
runTarget :: Node -> TargetM ()
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

    | Daemon {
        customOptions :: custom,
        _port :: Port
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
                  (progDesc "output target graph in dot format")) <>
        command "daemon"
            (info (Daemon <$> customParser <*> port)
                  (progDesc "start a daemon that exposes the store through a web API"))
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

    port :: Parser Port
    port = readNote "port not parseable" <$> strOption (
        value "8080" <>
        long "port" <>
        short 'p' <>
        help "port for the web API")
