{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module KrakenSpec (main, spec) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson (Object, FromJSON, ToJSON)
import           Data.Graph.Wrapper
import           Data.List
import           GHC.Generics
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           System.IO.Temp
import           Test.HUnit
import           Test.Hspec
import           Test.QuickCheck

import qualified System.Logging.Facade as Log

import           Kraken hiding (catch, runAsMain)
import qualified Kraken
import           Kraken.ActionMSpec (mockStateful)


main :: IO ()
main = hspec spec

runAsMain :: Store -> IO ()
runAsMain = Kraken.runAsMain "test program"

runWithExitCode :: Store -> IO ExitCode
runWithExitCode store = catch
  (Kraken.runAsMain "test program" store >> return ExitSuccess)
  (\ (e :: ExitCode) -> return e)

newtype ExampleConfig = ExampleConfig { foo :: Int } deriving Generic

instance ToJSON ExampleConfig
instance FromJSON ExampleConfig

spec :: Spec
spec = do

  describe "createStore" $ do
    it "does not store dependencies doubled" $ do
      let store = createStore $
            Target "t1" [] (return ()) Nothing :
            Target "t2" ["t1", "t1"] (return ()) Nothing :
            []
      edges (graphWithPriorities store) `shouldBe` [("t2", "t1")]

  describe "runAsMain" $ do
    describe "global --config file command line option" $ do
      it "reads kraken.conf.example successfully" $ do
        withArgs (words "check --config kraken.conf.example") $
          runAsMain (createStore [])

      it "allows to specify global options after the command" $ do
        withArgs (words "check --config kraken.conf.example") $ do
          runAsMain (createStore [])

      it "allows to specify a default config file (through the API)" $
        withSystemTempFile "kraken-tests" $ \ file handle -> do
          hClose handle
          readFile "kraken.conf.example" >>= writeFile file
          withArgs ["check"] $
            runAsMainWithCustomConfig "test program" file $
              \ (_ :: (FilePath, Object)) -> return (createStore [])

      it "reads the custom configuration section of the example file successfully" $ do
        withArgs (words "check --config kraken.conf.example") $
          runAsMainWithCustomConfig "test program" "kraken.conf.example" $ \ (_, config) -> do
            foo config `shouldBe` 42
            return $ createStore []

    describe "check command" $ do
      it "allows to perform static checks on the store" $ do
        let run = withArgs ["check"] $
              runWithExitCode $ createStore $
                Target "t1" ["t2"] (return ()) Nothing :
                []
        run `shouldThrow` (\ (e :: ErrorCall) -> show e == "target dependencies cannot be found: t2")

    describe "run command" $ do

      it "fails when given a non-existing target" $ do
        (output, exitCode) <- hCapture [stderr] $ withArgs (words "run foo") $
          runWithExitCode (createStore [])
        exitCode `shouldBe` ExitFailure 70
        output `shouldContain` "target not found: foo"

      it "allow to run monitors as targets" $ do
          result <- capture_ $ withArgs (words "run monitor") $ runAsMain $ createStore $
              Target "target" []
                  (error "target error")
                  (Just (Monitor "monitor" [] (const (liftIO $ putStrLn "monitor output")))) :
              []
          result `shouldContain` "monitor output"

      context "when run target cancels" $ do
        let store = createStore [Target "foo" [] (cancel "some error") Nothing]
            run = withArgs ["run", "foo"] (runWithExitCode store)

        it "writes error message to stderr twice (once during execution and once in a summary at the end)" $ do
          (output, exitCode) <- hCapture [stderr] run
          exitCode `shouldSatisfy` (/= ExitSuccess)
          output `shouldBe` unlines [
              "INFO: execution plan:"
            , "    foo"
            , "INFO: running target foo"
            , "ERROR: foo:"
            , "    some error"
            , "INFO: "
            , "FAILURE"
            , "-------"
            , "foo:"
            , "    some error"
            , ""
            ]

        it "exits with ExitFailure 70 (internal software error)" $ do
          hSilence [stderr] run `shouldReturn` ExitFailure 70

      context "when having dependencies" $ do
        let store mvar = createStore $
              Target "t1" [] (append mvar "t1") Nothing :
              Target "t2" ["t1"] (append mvar "t2") Nothing :
              []
            run mvar = withArgs (words "run t2") (runAsMain $ store mvar)
        it "runs the dependencies first" $ do
          mvar :: MVar [String] <- newMVar []
          (hSilence [stderr] (run mvar) >> readMVar mvar) `shouldReturn` ["t1", "t2"]

        it "doesn't run dependencies when given -x" $ do
          mvar :: MVar [String] <- newMVar []
          withArgs (words "run t2 -x") $ runAsMain $ store mvar
          readMVar mvar `shouldReturn` ["t2"]

      context "when given multiple targets with dependencies" $ do
        let store = createStore $
              Target "A" ["C"] (Log.info "executing A") Nothing :
              Target "B" ["C"] (Log.info "executing B") Nothing :
              Target "C" [] (Log.info "executing C") Nothing :
              []
            run :: IO String
            run = hCapture_ [stderr] $ withArgs (words "run A B") $ runAsMain store

        it "does execute every given target" $ do
          result <- run
          result `shouldContain` "executing A"
          result `shouldContain` "executing B"

        it "does not run one target multiple times" $ do
          result <- lines <$> run
          result `shouldSatisfy` (\ lines ->
            length (filter (== "INFO: executing C") lines) == 1)

      context "when one of multiple targets fails" $ do
        let targetList errorAction =
              Target "A" [] errorAction Nothing :
              Target "B" [] (Log.info "executing B") Nothing :
              Target "C" ["A", "B"] (Log.info "executing C") Nothing :
              []
            store = createStore . targetList

        it "runs all targets that don't depend on failing targets" $ do
          (output, exitCode) <- hCapture [stderr] $ withArgs (words "run C") $
            runWithExitCode $ store (cancel "error from A")
          exitCode `shouldBe` ExitFailure 70
          -- ensure "A" is run first
          output `shouldContain` unlines (
            "execution plan:" :
            "    A" :
            "    B" :
            "    C" :
            [])
          output `shouldContain` "executing B"

        it "runs all targets that don't depend on failing targets, with priorities in place" $ do
          let store = createStoreWithPriorities ["A","B","C"] . targetList
          (output, exitCode) <- hCapture [stderr] $ withArgs (words "run C") $
            runWithExitCode $ store (cancel "error from A")
          exitCode `shouldBe` ExitFailure 70
          -- ensure "A" is run first
          output `shouldContain` unlines (
            "execution plan:" :
            "    A" :
            "    B" :
            "    C" :
            [])
          output `shouldContain` "executing B"

        it "does not run targets that depend on failed targets" $ do
          output <- hCapture_ [stderr] $ withArgs (words "run C") $
            runWithExitCode $ store (cancel "error from A")
          output `shouldSatisfy` (not . ("executing C" `isInfixOf`))

        it "runs all targets that don't depend on failing targets even in case of exceptions" $ do
          output <- hCapture_ [stderr] $ withArgs (words "run C") $
            runWithExitCode $ store (error "error from A")
          output `shouldContain` "executing B"

        it "fails immediately after the first failing target when --fail-fast is given" $ do
          (output, exitCode) <- hCapture [stderr] $ withArgs (words "run C --fail-fast") $
            runWithExitCode $ store (cancel "error from A")
          output `shouldContain` "error from A"
          output `shouldSatisfy` (not . ("executing B" `isInfixOf`))
          exitCode `shouldBe` ExitFailure 70

      context "when having monitors" $ do
        let store :: MVar [String] -> TargetM () -> Store
            store mvar monitor = createStore $
              Target "t1" [] (append mvar "t1") (Just (Monitor "m1" [] (const monitor))) :
              []
        it "doesn't execute target when monitor runs successfully" $ do
          mvar <- newMVar []
          let run = withArgs ["run", "t1"] (runAsMain (store mvar (append mvar "m1")))
          hSilence [stderr] run
          readMVar mvar `shouldReturn` ["m1"]

        context "when the first run of the monitor complains" $ do
          it "re-runs the monitor after running the target" $ do
            mvar <- newMVar []
            let run = withArgs (words "run t1")
                  (runWithExitCode (store mvar (append mvar "m1" >> triggerTarget "m1 complains")))
            exitCode <- hSilence [stderr] run
            exitCode `shouldSatisfy` (/= ExitSuccess)
            readMVar mvar `shouldReturn` ["m1", "t1", "m1"]

          it "raises an error when the second run of the monitor complains" $ do
            mvar <- newMVar []
            let run = withArgs ["run", "t1"] (runWithExitCode (store mvar (triggerTarget "m1 complains")))
            hSilence [stderr] run `shouldReturn` ExitFailure 70

          it "runs successfully if the monitor does not complain the second time" $ do
            mvar <- newMVar []
            monitor <- stateDummy $
              (append mvar "m1.1" >> triggerTarget "m1 complains") :
              append mvar "m1.2" :
              []
            let run = withArgs (words "run t1") $ runAsMain $ store mvar monitor
            hSilence [stderr] run -- should not throw any exceptions
            readMVar mvar `shouldReturn` ["m1.1", "t1", "m1.2"]

          it "does not output the error message of the first monitor run like a normal error message" $ do
            mvar <- newMVar []
            monitor <- stateDummy $
              (triggerTarget "foo") :
              (return ()) :
              []
            output <- hCapture_ [stderr] $ withArgs (words "run t1") $ runAsMain $ store mvar monitor
            output `shouldSatisfy` (not . (showError (Error Nothing "foo") `isInfixOf`))
            output `shouldSatisfy` (not . (showError (Error (Just "m1") "foo") `isInfixOf`))

          it "does not run the target if the first run of the monitor throws an Exception (instead of abort)" $ do
            mvar <- newMVar []
            _ <- hSilence [stdout, stderr] $
              withArgs (words "run t1") $ runWithExitCode (store mvar (error "m1 raises an Exception"))
            readMVar mvar `shouldReturn` []

        it "doesn't execute monitors when called with --omit-monitors" $ do
          mvar <- newMVar []
          let run = withArgs ["run", "--omit-monitors", "t1"] (runAsMain (store mvar (append mvar "m1")))
          hSilence [stderr] run
          readMVar mvar `shouldReturn` ["t1"]

      context "--retry-on-failure" $ do
        it "reruns failing targets" $ do
          mvar :: MVar [String] <- newMVar []
          failingOnce <- mockStateful $
            const (append mvar "failing" >> cancel "cancel") :
            const (append mvar "success") :
            []
          let store = createStore $
                Target "t1" [] (failingOnce ()) Nothing :
                []
              run = withArgs (words "run --retry-on-failure t1") (runWithExitCode store)
          run `shouldReturn` ExitFailure 70
          readMVar mvar `shouldReturn` ["failing", "success"]

        it "does not retry indefinitely" $ do
          let store = createStore $
                Target "t1" [] (cancel "error") Nothing :
                []
              run = withArgs (words "run --retry-on-failure t1") (runWithExitCode store)
          run `shouldReturn` ExitFailure 70

      context "-e --exclude with multiple targets" $ do
        let store = createStore $
              Target "A" [] (Log.info "executing A") Nothing :
              Target "B" ["A"] (Log.info "executing B") Nothing :
              Target "C" ["B"] (Log.info "executing C") Nothing :
              Target "D" ["B"] (Log.info "executing D") Nothing :
              Target "E" ["D"] (Log.info "executing E") Nothing :
              []
            run :: IO String
            run = hCapture_ [stderr] $ withArgs (words "run C E -e D -e A") $ runAsMain store

        it "does execute all dependencies targets not in excluded list" $ do
          result <- run
          result `shouldContain` "executing B"
          result `shouldContain` "executing C"
          result `shouldContain` "executing E"

        it "does not execute excluded targets" $ do
          result <- lines <$> run
          result `shouldSatisfy` (\r -> not $ any (`elem` r) ["executing A", "executing D"])

      context "using addPriorities" $ do
        let store mvar priorities = createStoreWithPriorities priorities $
              Target "a" [] (append mvar "a") Nothing :
              Target "b" ["a"] (append mvar "b") Nothing :
              Target "c" ["a"] (append mvar "c") Nothing :
              Target "d" ["b", "c"] (append mvar "d") Nothing :
              []
            run :: [TargetName] -> IO [String]
            run priorities = do
              mvar <- newMVar []
              withArgs (words "run d") $ runAsMain $
                (store mvar priorities)
              readMVar mvar
        it "honors supplied priorities" $ do
          run ["b", "c"] `shouldReturn` words "a b c d"
          run ["c", "b"] `shouldReturn` words "a c b d"
          run ["a", "b", "c"] `shouldReturn` words "a b c d"

        it "reports cycles in case of impossible priorities" $ do
          run ["b", "a"] `shouldThrow` (\ (ErrorCall message) -> "cycles" `isInfixOf` message)

    describe "list command" $ do
      it "lists available targets" $ do
        withArgs ["list"] $ do
          let store = createStore [
                  Target "foo" [] (return ()) Nothing
                , Target "bar" [] (return ()) Nothing
                , Target "baz" [] (return ()) Nothing
                ]
          capture_ (runAsMain store) `shouldReturn` (unlines . sort) ["foo", "bar", "baz"]

    describe "dot command" $ do
      it "includes all nodes in dot output" $ do
        property $ \ (nub -> nodes) -> do
          withArgs ["dot"] $ do
            let store = createStore
                    (map (\ name -> Target name [] (return ()) Nothing) nodes)
            output <- capture_ (runAsMain store)
            output `shouldSatisfy` \ output ->
                all (\ (TargetName node) -> node `isInfixOf` output) nodes

      it "produces output target graph in dot format" $ do
        withArgs ["dot"] $ do
          let store = createStore [
                  Target "foo" ["bar"] (return ()) Nothing
                , Target "bar" [] (return ()) Nothing
                ]
          (unwords . words <$> capture_ (runAsMain store)) `shouldReturn` unwords [
              "digraph targets {"
            , "rankdir = RL;"
            , "\"bar\""
            , "[shape = oval];"
            , "\"foo\""
            , "[shape = oval];"
            , "\"foo\" -> \"bar\" [color = \"black\"];"
            , "}"
            ]

      context "when used with multiple prefix options" $ do

        it "removes the longest matching prefix" $ do
          withArgs ["dot", "-p", "prefix", "-p", "prefixlong"] $ do
            let store = createStore $
                  Target "prefix_t1" [] (return ()) Nothing :
                  Target "prefixlong_t2" [] (return ()) Nothing :
                  []
            output <- capture_ (runAsMain store)
            when ("long" `isInfixOf` output) $
                assertFailure ("long is in output:\n" ++ output)

        it "doesn't strip but abbreviates prefixes" $ do
          let store = createStore $
                Target "foobar.1" [] (return ()) Nothing :
                Target "fuubar.2" [] (return ()) Nothing :
                []
          output <- withArgs (words "dot -p foobar. -p fuubar.") $ capture_ (runAsMain store)
          output `shouldContain` "\"fo.1\""
          output `shouldContain` "\"fu.2\""

stateDummy :: MonadIO m => [m a] -> IO (m a)
stateDummy l = do
    ref <- newMVar l
    return $ do
        next <- liftIO $ modifyMVar ref $ \ (a : r) -> return (r, a)
        next

append :: MonadIO m => MVar [a] -> a -> m ()
append mvar n = liftIO $ modifyMVar_ mvar $ \ r -> return (r ++ [n])

instance Arbitrary TargetName where
    arbitrary = TargetName <$> listOf1 (elements ['a' .. 'z'])
    shrink = map TargetName . shrink . show
