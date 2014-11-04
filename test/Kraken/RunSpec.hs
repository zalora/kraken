{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Kraken.RunSpec where


import           Control.Exception
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec

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
        lines output `shouldContain` ["retrying target A"]

      it "waits the specified delay if given" $ do
        let waitConfig = config {retryDelay = Just 0.01}
        (output, _) <- run retryOptions waitConfig =<< retryStore
        lines output `shouldContain` ["retrying target A in 0.01 seconds..."]
