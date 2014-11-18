{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Kraken.RunSpec where


import           Control.Exception
import           Control.Monad.IO.Class
import           Prelude                hiding (log)
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec
import           Test.QuickCheck

import           Kraken.ActionM
import           Kraken.Config
import           Kraken.Graph
import           Kraken.Run
import           Kraken.Store

import           Kraken.ActionMSpec


run :: Options -> KrakenConfig -> Store -> IO (String, ExitCode)
run options config store =
  hCapture [stderr] $
  Control.Exception.catch
    (Kraken.Run.runStore options config store >> return ExitSuccess)
    return

options :: TargetName -> RunOptions
options target = RunOptions {
  targetList = SelectedTargets [target],
  dryRun = False,
  useAsPrefix = False,
  dontChaseDependencies = False,
  omitMonitors = False,
  failFast = False,
  retryOnFailure = False,
  excludeTargets = []
 }

config :: KrakenConfig
config = defaultKrakenConfig


spec :: Spec
spec = do
  describe "runStore" $ do
    context "retryOnFailure" $ do

      let retryStore = do
            failingOnce :: () -> TargetM () <- mockStateful $
              const (cancel "cancel") :
              const (return ()) :
              []
            return $ createStore $
              Target "A" [] (failingOnce ()) Nothing :
              []

          retryOptions :: Options
          retryOptions = Run ((options "A"){retryOnFailure = True})

      it "doesn't wait if no delay is given" $ do
        (output, _) <- run retryOptions config =<< retryStore
        lines output `shouldContain` ["INFO: retrying target A"]

      it "waits the specified delay if given" $ do
        let waitConfig = config {retryDelay = Just 0.01}
        (output, _) <- run retryOptions waitConfig =<< retryStore
        lines output `shouldContain` ["INFO: retrying target A in 0.01 seconds..."]

      it "retries n times if specified" $ do
        property $ \ (Positive numberOfFails) (Positive numberOfRetries) -> ioProperty $ do
          let waitConfig = config {numberOfRetries = numberOfRetries}
          failing <- mockStateful $
            replicate numberOfFails (const (cancel "cancel")) ++
            const (return ()) :
            []
          let store = createStore $
                Target "A" [] (log "A is executed" >> failing ()) Nothing :
                Target "B" ["A"] (log "B is executed") Nothing :
                []
          (output, exitCode) <- run (Run (options "B"){retryOnFailure = True}) waitConfig store
          return $
            (exitCode === if numberOfFails > 0 then ExitFailure 70 else ExitSuccess) .&&.
            (counterexample "is A executed the right number of times?"
              (length (filter (== "A is executed") (lines output))
               === min (1 + numberOfRetries) (1 + numberOfFails))) .&&.
            (counterexample ("is B executed? - " ++ output)
              (if numberOfFails <= numberOfRetries
                then "B is executed" `elem` lines output
                else all (not . (== "B is executed")) (lines output)))

log :: MonadIO m => String -> m ()
log message = liftIO $ hPutStrLn stderr message
