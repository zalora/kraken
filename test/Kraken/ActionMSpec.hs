{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Kraken.ActionMSpec where


import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Monoid
import           System.IO
import           System.IO.Silently
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Property (morallyDubiousIOProperty)

import           Kraken.ActionM
import           Kraken.Util


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "withTargetName" $ do

        it "includes the given target name in error messages" $ do
            Left result <- runActionM (withTargetName "fooTarget" $ cancel "bar")
            concatMap showError result `shouldContain` "fooTarget"

        context "works in conjunction with Exceptions converted to lefts through isolate" $ do
            it "works when wrapping isolate" $ do
                Left result <- runActionM $ withTargetName "target name" $ isolate $ do
                    liftIO $ throwIO $ ErrorCall "error message"
                result `shouldBe` [Error (Just "target name") "error message"]

            it "works when being wrapped by isolate" $ do
                Left result <- runActionM $ isolate $ withTargetName "target name" $ do
                    liftIO $ throwIO $ ErrorCall "error message"
                result `shouldBe` [Error (Just "target name") "error message"]

    describe "logError" $ do

        it "writes errors to stderr" $ do
            output <- hCapture_ [stderr] $ runActionM (logError "foo")
            output `shouldContain` "foo"

        it "includes error in the error summary" $ do
            runActionM (logError "foo")
                `shouldReturn` Left [Error Nothing "foo"]

        it "does not shortcut the monadic action" $ do
            output <- capture_ $ runActionM (logError "foo" >> liftIO (putStrLn "bar"))
            output `shouldContain` "bar"

    describe "cancel" $ do

        it "reports calls to cancel as Lefts" $ do
            runActionM (cancel "foo" :: TargetM ()) `shouldReturn` Left [Error Nothing "foo"]

        it "writes calls to cancel to stderr" $ do
            output <- hCapture_ [stderr] $ runActionM (cancel "foo")
            output `shouldContain` "foo"

        it "does not execute subsequent actions" $ do
            output <- capture_ $ runActionM (cancel "foo" >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` (not . ("bar" `isInfixOf`))

    describe "mapExceptions" $ do
        it "allows to transform exceptions" $ do
            runActionM (mapExceptions (\ (ErrorCall x) -> ErrorCall (reverse x)) (error "bla"))
                `shouldThrow` (\ (e :: ErrorCall) -> show e == "alb")

    describe "isolate" $ do

        it "does execute subsequent actions in case of cancels" $ do
            output <- capture_ $ runActionM (isolate (cancel "foo") >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` ("bar" `isInfixOf`)

        it "does execute subsequent actions in case of Exceptions" $ do
            output <- capture_ $ runActionM (isolate (error "foo") >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` ("bar" `isInfixOf`)

        it "does collect errors" $ do
            result <- runActionM (isolate (cancel "foo") >> (cancel "bar" :: TargetM ()))
            result `shouldBe` Left [Error Nothing "foo", Error Nothing "bar"]

        it "prints cancel to stderr" $ do
            output <- hCapture_ [stderr] $ runActionM $
                isolate (cancel "foo") >>
                liftIO (hPutStrLn stderr "bar")
            output `shouldBe` "<no target>:\n    foo\nbar\n"

        it "prints exceptions to stderr" $ do
            output <- hCapture_ [stderr] $ runActionM (isolate (error "foo"))
            output `shouldContain` "foo"

        it "converts exceptions to Lefts" $ do
            runActionM (isolate (error "foo")) `shouldReturn`
                Left [Error Nothing "foo"]

        it "uses the outer currentTarget" $ do
            result <- runActionM $ withTargetName "foo" $ isolate $
                cancel "bar"
            result `shouldBe` Left [Error (Just "foo") "bar"]

        it "(isolate . cancel) == logError" $ do
            let run action = hCapture [stdout, stderr] $ runActionM $ action
            a <- run (isolate (cancel "foo"))
            b <- run (logError "foo")
            a `shouldBe` b

        testBatch $ monoid (error "proxy" :: IsolatedTargetM)

    context "monitors" $ do
        describe "triggerTarget" $ do
            it "equals '(cancel . (\"message from monitor: \" ++))' when used \
               \outside 'bracketWithMonitor'" $ do
                let msg = "foo"
                a <- runActionM (triggerTarget msg :: TargetM ())
                b <- runActionM ((cancel . ("message from monitor: " ++)) msg)
                a `shouldBe` b

        describe "bracketWithMonitor" $ do

            it "doesn't run the bracketed action if the opening monitors cancels" $ do
                output <- capture_ $ runActionM $ bracketWithMonitor
                    (const (cancel "monitor cancels"))
                    (liftIO $ putStrLn "foo")
                output `shouldSatisfy` (not . ("foo" `isInfixOf`))

            it "runs the bracketed action when the opening monitor uses triggerTarget" $ do
                output <- capture_ $ runActionM $ bracketWithMonitor
                    (\ _ -> triggerTarget "foo")
                    (liftIO $ putStrLn "bar")
                output `shouldContain` "bar"

            it "outputs the triggerTarget message from the opening monitor to stderr" $ do
                output <- hCapture_ [stderr] $ runActionM $ bracketWithMonitor
                    (\ _ -> triggerTarget "foo")
                    (return ())
                output `shouldContain` "foo"

            it "doesn't include the triggerTarget message from the opening monitor in \
               \the summary" $ do
                monitor <- mockStateful (const (triggerTarget "foo") : const (return ()) : [])
                result <- runActionM $ bracketWithMonitor
                    monitor
                    (return ())
                result `shouldBe` Right ()

            it "fails when the closing monitor uses triggerTarget" $ do
                monitor <- mockStateful $
                    const (triggerTarget "foo") :
                    const (triggerTarget "bar") :
                    []
                result <- runActionM $ bracketWithMonitor monitor (return ())
                result `shouldBe` Left [Error Nothing "message from monitor: bar"]

            it "succeeds when the closing monitor succeeds" $ do
                monitor <- mockStateful $
                    const (triggerTarget "foo") :
                    const (return ()) :
                    []
                result <- runActionM $ bracketWithMonitor
                    monitor
                    (return ())
                result `shouldBe` Right ()

        describe "memoizeMonitorInput" $ do
            it "allows to memoize monitor inputs when used in the monitor in \
               \bracketWithMonitor" $ do
                inputMVar <- newMVar (1 :: Integer)
                outputMVar <- newMVar (0 :: Integer)
                let monitor memoized = do
                        input <- memoizeMonitorInput memoized $ liftIO $ readMVar inputMVar
                        output <- liftIO $ readMVar outputMVar
                        when (input > output) (outOfDate (show (input, output)) input)
                    targetAction = liftIO $ do
                        -- updating the output
                        input <- readMVar inputMVar
                        modifyMVar_ outputMVar $ const $ return input
                        -- The input changes during the target action
                        modifyMVar_ inputMVar $ \ input -> return (succ input)
                result <- runActionM $ bracketWithMonitor monitor targetAction
                result `shouldBe` Right ()


-- | Helps to mock stateful operations.
mockStateful :: MonadIO m => [i -> m a] -> IO (i -> m a)
mockStateful actions = do
    mvar <- newMVar actions
    return $ \ i -> do
        action <- liftIO $ modifyMVar mvar $ \ (a : r) -> return (r, a)
        action i


-- | Deeply embedded DSL for (a subset of) TargetM ().
data IsolatedTargetM
    = Cancel String
    | ReturnUnit
    | LogMessageLn String
    | TriggerTarget String
    | OutOfDate String Int

    | IsolatedTargetM :>> IsolatedTargetM
  deriving Show

unwrap :: IsolatedTargetM -> ActionM Int ()
unwrap x = isolate $ case x of
    Cancel s -> cancel s
    ReturnUnit -> return ()
    LogMessageLn s -> logMessageLn s
    TriggerTarget s -> triggerTarget s
    OutOfDate s n -> outOfDate s n
    (a :>> b) -> unwrap a >> unwrap b

instance Monoid IsolatedTargetM where
    mempty = ReturnUnit
    mappend = (:>>)

instance EqProp IsolatedTargetM where
    a =-= b = morallyDubiousIOProperty $ do
      resultA <- hCapture [stdout, stderr] $ runActionM $ unwrap a
      resultB <- hCapture [stdout, stderr] $ runActionM $ unwrap b
      return $
          printTestCase (show resultA ++ "\n/=\n" ++ show resultB) $
          resultA == resultB

instance Arbitrary IsolatedTargetM where
    arbitrary = oneof $
        (Cancel <$> arbitrary) :
        (return ReturnUnit) :
        (LogMessageLn <$> arbitrary) :
        (TriggerTarget <$> arbitrary) :
        (OutOfDate <$> arbitrary <*> arbitrary) :
        ((:>>) <$> arbitrary <*> arbitrary) :
        []
    shrink (Cancel s) = map Cancel $ shrink s
    shrink ReturnUnit = []
    shrink (LogMessageLn s) = map LogMessageLn $ shrink s
    shrink (TriggerTarget s) =
        [TriggerTarget s' | s' <- shrink s]
    shrink (OutOfDate s n) =
        [OutOfDate s' n | s' <- shrink s] ++
        [OutOfDate s n' | n' <- shrink n]
    shrink (a :>> b) =
        [a' :>> b | a' <- shrink a] ++
        [a :>> b' | b' <- shrink b] ++
        [a, b]
