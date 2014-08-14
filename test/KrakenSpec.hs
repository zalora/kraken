{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module KrakenSpec (main, spec) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Graph.Wrapper
import           Data.List
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck

import           Kraken                  hiding (catch, runAsMain)
import qualified Kraken


main :: IO ()
main = hspec spec

runAsMain :: Store -> IO ()
runAsMain = Kraken.runAsMain "test program"

runWithExitCode :: Store -> IO ExitCode
runWithExitCode store = catch
  (Kraken.runAsMain "test program" store >> return ExitSuccess)
  (\ (e :: ExitCode) -> return e)

spec :: Spec
spec = do

  describe "createStore" $ do
    it "does not store dependencies doubled" $ do
      let store = createStore $
            Target "t1" [] Nothing (return ()) :
            Target "t2" ["t1", "t1"] Nothing (return ()) :
            []
      edges (graph store) `shouldBe` [("t2", "t1")]

  describe "runAsMain" $ do
    describe "run command" $ do

      it "fails when given a non-existing target" $ do
        (output, exitCode) <- hCapture [stderr] $ withArgs (words "run foo") $
          runWithExitCode (createStore [])
        exitCode `shouldBe` ExitFailure 70
        output `shouldContain` "target not found: foo"

      context "when run target fails" $ do
        let store = createStore [Target "foo" [] Nothing $ abort "some error"]
            run = withArgs ["run", "foo"] (runWithExitCode store)

        it "writes error message to stderr twice \
           \(once during execution and once in a summary at the end)" $ do
          (output, exitCode) <- hCapture [stderr] run
          exitCode `shouldSatisfy` (/= ExitSuccess)
          output `shouldBe` unlines [
              "execution plan:"
            , "    foo"
            , "running target foo"
            , "foo:"
            , "    some error"
            , ""
            , "FAILURE"
            , "-------"
            , "foo:"
            , "    some error"
            ]

        it "exits with ExitFailure 70 (internal software error)" $ do
          hSilence [stderr] run `shouldReturn` ExitFailure 70

      context "when having dependencies" $ do
        let store mvar = createStore $
              Target "t1" [] Nothing (append mvar "t1") :
              Target "t2" ["t1"] Nothing (append mvar "t2") :
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
              Target "A" ["C"] Nothing (logMessageLn "executing A") :
              Target "B" ["C"] Nothing (logMessageLn "executing B") :
              Target "C" [] Nothing (logMessageLn "executing C") :
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
            length (filter (== "executing C") lines) == 1)

      context "when one of multiple targets fails" $ do
        let store errorAction = createStore $
              Target "A" [] Nothing errorAction :
              Target "B" [] Nothing (logMessageLn "executing B") :
              Target "C" ["A", "B"] Nothing (logMessageLn "executing C") :
              []
        it "runs all targets that don't depend on failing targets" $ do
          (output, exitCode) <- hCapture [stderr] $ withArgs (words "run C") $
            runWithExitCode $ store (abort "error from A")
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
            runWithExitCode $ store (abort "error from A")
          output `shouldSatisfy` (not . ("executing C" `isInfixOf`))

        it "runs all targets that don't depend on failing targets even in case of exceptions" $ do
          output <- hCapture_ [stderr] $ withArgs (words "run C") $
            runWithExitCode $ store (error "error from A")
          output `shouldContain` "executing B"

        it "fails immediately after the first failing target when --fail-fast is given" $ do
          (output, exitCode) <- hCapture [stderr] $ withArgs (words "run C --fail-fast") $
            runWithExitCode $ store (abort "error from A")
          output `shouldContain` "error from A"
          output `shouldSatisfy` (not . ("executing B" `isInfixOf`))
          exitCode `shouldBe` ExitFailure 70

      context "when having monitors" $ do
        let store :: MVar [String] -> TargetM () -> Store
            store mvar monitor = createStore $
              Target "t1" [] (Just "m1") (append mvar "t1") :
              Target "m1" [] Nothing monitor :
              []
        it "doesn't execute target when monitor runs successfully" $ do
          mvar <- newMVar []
          let run = withArgs ["run", "t1"] (runAsMain (store mvar (append mvar "m1")))
          hSilence [stderr] run
          readMVar mvar `shouldReturn` ["m1"]

        context "when the first run of the monitor complains" $ do
          it "re-runs the monitor after running the target" $ do
            mvar <- newMVar []
            let run = withArgs (words "run t1") (runWithExitCode (store mvar (append mvar "m1" >> abort "m1 complains")))
            exitCode <- hSilence [stderr] run
            exitCode `shouldSatisfy` (/= ExitSuccess)
            readMVar mvar `shouldReturn` ["m1", "t1", "m1"]

          it "raises an error when the second run of the monitor complains" $ do
            mvar <- newMVar []
            let run = withArgs ["run", "t1"] (runWithExitCode (store mvar (abort "m1 complains")))
            hSilence [stderr] run `shouldReturn` ExitFailure 70

          it "runs successfully if the monitor does not complain the second time" $ do
            mvar <- newMVar []
            monitor <- stateDummy $
              (append mvar "m1.1" >> abort "m1 complains") :
              append mvar "m1.2" :
              []
            let run = withArgs (words "run t1") $ runAsMain $ store mvar monitor
            hSilence [stderr] run -- should not throw any exceptions
            readMVar mvar `shouldReturn` ["m1.1", "t1", "m1.2"]

          it "does not output the error message of the first monitor run like a normal error message" $ do
            mvar <- newMVar []
            monitor <- stateDummy $
              (abort "foo") :
              (return ()) :
              []
            output <- hCapture_ [stderr] $ withArgs (words "run t1") $ runAsMain $ store mvar monitor
            output `shouldSatisfy` (not . (showFailure (Nothing, "foo") `isInfixOf`))
            output `shouldSatisfy` (not . (showFailure (Just "m1", "foo") `isInfixOf`))

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

    describe "list command" $ do
      it "lists available targets" $ do
        withArgs ["list"] $ do
          let store = createStore [
                  Target "foo" [] Nothing $ return ()
                , Target "bar" [] Nothing $ return ()
                , Target "baz" [] Nothing $ return ()
                ]
          capture_ (runAsMain store) `shouldReturn` (unlines . sort) ["foo", "bar", "baz"]

    describe "dot command" $ do
      it "includes all nodes in dot output" $ do
        property $ \ (nub -> nodes) -> do
          withArgs ["dot"] $ do
            let store = createStore
                    (map (\ name -> Target name [] Nothing (return ())) nodes)
            output <- capture_ (runAsMain store)
            output `shouldSatisfy` \ output ->
                all (\ (TargetName node) -> node `isInfixOf` output) nodes

      it "produces output target graph in dot format" $ do
        withArgs ["dot"] $ do
          let store = createStore [
                  Target "foo" ["bar"] Nothing $ return ()
                , Target "bar" [] Nothing $ return ()
                ]
          (unwords . words <$> capture_ (runAsMain store)) `shouldReturn` unwords [
              "digraph targets {"
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
                  Target "prefix_t1" [] Nothing (return ()) :
                  Target "prefixlong_t2" [] Nothing (return ()) :
                  []
            output <- capture_ (runAsMain store)
            when ("long" `isInfixOf` output) $
                assertFailure ("long is in output:\n" ++ output)

        it "doesn't strip but abbreviates prefixes" $ do
          let store = createStore $
                Target "foobar.1" [] Nothing (return ()) :
                Target "fuubar.2" [] Nothing (return ()) :
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
