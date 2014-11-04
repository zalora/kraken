{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TupleSections #-}

module Kraken.Run (
  runAsMain,
  runAsMainWithCustomConfig,

  -- exported for testing
  runStore,
  Options(..),
  RunOptions(..),
 ) where


import           Control.Concurrent
import           Control.Monad            (when)
import           Control.Monad.IO.Class
import           Data.Configurator.Types  (Config)
import           Data.Foldable            (forM_)
import           Data.Graph.Wrapper       as Graph hiding (toList)
import           Data.List                as List (null, (\\))
import           Data.Maybe
import           Data.Monoid
import           Data.Set                 as Set (empty, insert, member)
import           Data.String.Interpolate
import           Network.Wai.Handler.Warp hiding (cancel)
import           Options.Applicative      hiding (action)
import           Prelude                  hiding (mapM)
import           Safe
import           System.Exit
import           System.IO
import           Text.Printf

import           Kraken.ActionM
import           Kraken.Config
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
    (options, _) <- execParser (options description)
    runStore options defaultKrakenConfig store

runAsMainWithCustomConfig :: String -> FilePath -> ((FilePath, Config) -> IO Store) -> IO ()
runAsMainWithCustomConfig description defaultConfigFile mkStore = do
    (options, mConfigFile) <- execParser (options description)
    let configFile = fromMaybe defaultConfigFile mConfigFile
    (config, custom) <- loadConfig configFile
    store <- mkStore (configFile, custom)
    runStore options config store

runStore :: Options -> KrakenConfig -> Store -> IO ()
runStore opts krakenConfig store = case opts of
    Run runOptions -> do
        result <- runActionM $ do
            targets <- lookupTargets store (useAsPrefix runOptions) (targetList runOptions)
            runTargets runOptions krakenConfig store targets
        either reportAndExit return result
    Check -> do
        -- KrakenConfig is already parsed.
        -- Checks are already performed by 'createStore'.
        evalStore store
        logMessageLn "Store is consistent."
    List -> putStr $ unlines $
        fmap show $
        reverse $ topologicalSort $
        graph store
    Dot withMonitors prefixes transitiveReduction ->
        putStr $ toDot withMonitors prefixes transitiveReduction $
          fmap Kraken.Dot.fromNode $ graph store
    Daemon port -> runDaemon port store
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
runTargets :: RunOptions -> KrakenConfig -> Store -> [TargetName] -> TargetM ()
runTargets options config store targets = do
    plans <- lookupExecutionPlan store (dontChaseDependencies options) targets
    let executionPlan = filter (not . (`elem` excludeTargets options) . fst) plans
    logMessage . unlines $
        "execution plan:" :
        (fmap (("    " ++) . show . fst) executionPlan)
    when (not $ dryRun options) $ do
        doneTargets <- liftIO $ newMVar Set.empty
        forM_ executionPlan $ \ (name, node) -> do
            done <- liftIO $ readMVar doneTargets
            let dependencies = lookupDependencies store name List.\\ excludeTargets options
            when (all (`Set.member` done) dependencies || (dontChaseDependencies options)) $ do
                isolateM name $ do
                    runTargetWithMonitor (omitMonitors options) (name, node)
                    liftIO $ modifyMVar_ doneTargets (return . insert name)
  where
    isolateM :: TargetName -> TargetM () -> TargetM ()
    isolateM target action = if failFast options
        then action
        else do
          result <- isolate action
          case (retryOnFailure options, result) of
            (True, IsolateFailure) -> do
                case retryDelay config of
                    Nothing -> do
                        logMessageLn [i|retrying target #{target}|]
                    Just delay -> do
                        logMessageLn [i|retrying target #{target} in #{printf "%f" delay :: String} seconds...|]
                        liftIO $ threadDelay $ round (delay * 10 ^ (6 :: Integer))
                _ <- isolate action
                return ()
            _ -> return ()

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
runTargetWithMonitor :: Bool -> (TargetName, Node) -> TargetM ()
runTargetWithMonitor _omitMonitors target@(_, Node{nodeMonitor = Nothing}) =
    runTarget target
runTargetWithMonitor True target =
    runTarget target
runTargetWithMonitor False target@(targetName, Node{nodeMonitor = Just (NodeMonitor monitorName monitorAction)}) =
  withTargetName targetName $ do
    logMessageLn [i|running monitor for #{targetName}|]

    bracketWithMonitor
        (\ monitorInput -> withTargetName monitorName (monitorAction monitorInput))
        (runTarget target)


-- | Runs the target ignoring dependencies and monitors.
runTarget :: (TargetName, Node) -> TargetM ()
runTarget (targetName, target) = withTargetName targetName $ do
    logMessageLn [i|running target #{targetName}|]
    nodeAction target


-- * cli options

data Options
    = Run RunOptions
    | Check
    | List
    | Dot {
        _withMonitors :: Bool,
        _prefixes :: Maybe [String],
        _transitiveReduction :: Bool
      }

    | Daemon {
        _port :: Port
      }
  deriving Show

data RunOptions = RunOptions {
    targetList :: TargetList,
    dryRun :: Bool,
    useAsPrefix :: Bool,
    dontChaseDependencies :: Bool,
    omitMonitors :: Bool,
    failFast :: Bool,
    retryOnFailure :: Bool,
    excludeTargets :: [TargetName]
  }
    deriving Show

options :: String -> ParserInfo (Options, Maybe FilePath)
options description =
    info (helper <*> parser) (fullDesc <> progDesc description)
  where
    parser :: Parser (Options, Maybe FilePath)
    parser = hsubparser (
        command "run"
            (info (addConfig (Run <$> (RunOptions <$>
                        targetList <*>
                        dryRun <*>
                        useAsPrefix <*>
                        dontChaseDependencies <*>
                        omitMonitors <*>
                        failFast <*>
                        retryOnFailure <*>
                        excludeTargets)))
                (progDesc "run creation and monitoring operations for the specified targets")) <>
        command "check"
            (info (addConfig (pure Check)) (progDesc "perform static checks on the target store")) <>
        command "list"
            (info (addConfig (pure List)) (progDesc "list all targets")) <>
        command "dot"
            (info (addConfig (Dot <$> withMonitors <*> prefixes <*> transitiveReduction))
                (progDesc "output target graph in dot format")) <>
        command "daemon"
            (info (addConfig (Daemon <$> port))
                (progDesc "start a daemon that exposes the store through a web API"))
      )


    addConfig :: Parser a -> Parser (a, Maybe FilePath)
    addConfig fst = (,) <$> fst <*> config

    config :: Parser (Maybe FilePath)
    config = optional $ strOption $
        long "config" <>
        metavar "FILE" <>
        help "config file"

    targetList :: Parser TargetList
    targetList = toTargetList <$>
         some (strArgument $
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

    retryOnFailure :: Parser Bool
    retryOnFailure = switch (
        long "retry-on-failure" <>
        help "will retry the whole process on failure")

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

    excludeTargets :: Parser [TargetName]
    excludeTargets = map TargetName <$>
        many ( strOption $
          short 'e' <>
          long "exclude" <>
          metavar "TARGET" <>
          help "Targets to be excluded")
