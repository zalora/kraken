{-# LANGUAGE OverloadedStrings #-}

module Kraken.TargetMSpec where


import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
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

import           Kraken.TargetM
import           Kraken.Util


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "fail" $ do

        it "reports calls to fail as Lefts" $ do
            runTargetM (fail "foo" :: TargetM ()) `shouldReturn` Left [Error Nothing "foo"]

        it "writes calls to fail to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM (fail "foo")
            output `shouldContain` "foo"

        it "does not execute subsequent actions" $ do
            output <- capture_ $ runTargetM (fail "foo" >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` (not . ("bar" `isInfixOf`))

    describe "mapExceptions" $ do
        it "allows to transform exceptions" $ do
            runTargetM (mapExceptions (\ (ErrorCall x) -> ErrorCall (reverse x)) (error "bla"))
                `shouldThrow` (== ErrorCall "alb")

    describe "isolate" $ do

        it "does execute subsequent actions in case of fails" $ do
            output <- capture_ $ runTargetM (isolate (fail "foo") >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` ("bar" `isInfixOf`)

        it "does execute subsequent actions in case of Exceptions" $ do
            output <- capture_ $ runTargetM (isolate (error "foo") >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` ("bar" `isInfixOf`)

        it "does collect errors" $ do
            result <- runTargetM (isolate (fail "foo") >> (fail "bar" :: TargetM ()))
            result `shouldBe` Left [Error Nothing "foo", Error Nothing "bar"]

        it "prints fail to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM $
                isolate (fail "foo") >>
                liftIO (hPutStrLn stderr "bar")
            output `shouldBe` "<no target>:\n    foo\nbar\n"

        it "prints exceptions to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM (isolate (error "foo"))
            output `shouldContain` "foo"

        it "converts exceptions to Lefts" $ do
            runTargetM (isolate (error "foo")) `shouldReturn`
                Left [Error Nothing "foo"]

        it "uses the outer currentTarget" $ do
            result <- runTargetM $ withTargetName "foo" $ isolate $
                fail "bar"
            result `shouldBe` Left [Error (Just "foo") "bar"]

        testBatch $ monoid (error "proxy" :: IsolatedTargetM)

    describe "outOfDate" $ do
        it "equals '(fail . (\"message from monitor: \" ++))' when used \
           \outside 'bracketWithMonitor'" $ do
            let msg = "foo"
            a <- runTargetM (outOfDate msg :: TargetM ())
            b <- runTargetM ((fail . ("message from monitor: " ++)) msg)
            a `shouldBe` b

    describe "bracketWithMonitor" $ do

        it "doesn't run the bracketed action if the opening monitors fails" $ do
            output <- capture_ $ runTargetM $ bracketWithMonitor
                (fail "monitor fails")
                (liftIO $ putStrLn "foo")
            output `shouldSatisfy` (not . ("foo" `isInfixOf`))

        it "runs the bracketed action when the opening monitor uses outOfDate" $ do
            output <- capture_ $ runTargetM $ bracketWithMonitor
                (outOfDate "foo")
                (liftIO $ putStrLn "bar")
            output `shouldContain` "bar"

        it "outputs the outOfDate message from the opening monitor to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM $ bracketWithMonitor
                (outOfDate "foo")
                (return ())
            output `shouldContain` "foo"

        it "doesn't include the outOfDate message from the opening monitor in \
           \the summary" $ do
            monitor <- mockStateful (outOfDate "foo" : return () : [])
            result <- runTargetM $ bracketWithMonitor
                monitor
                (return ())
            result `shouldBe` Right ()

        it "fails when the closing monitor uses outOfDate" $ do
            monitor <- mockStateful $
                outOfDate "foo" :
                outOfDate "bar" :
                []
            result <- runTargetM $ bracketWithMonitor monitor (return ())
            result `shouldBe` Left [Error Nothing "message from monitor: bar"]

        it "succeeds when the closing monitor succeeds" $ do
            monitor <- mockStateful $
                outOfDate "foo" :
                return () :
                []
            result <- runTargetM $ bracketWithMonitor
                monitor
                (return ())
            result `shouldBe` Right ()

    describe "withTargetName" $ do

        it "includes the given target name in error messages" $ do
            Left result <- runTargetM (withTargetName "fooTarget" $ fail "bar")
            concatMap showError result `shouldContain` "fooTarget"


-- | Helps to mock stateful operations.
mockStateful :: MonadIO m => [m a] -> IO (m a)
mockStateful actions = do
    mvar <- newMVar actions
    return $ do
        action <- liftIO $ modifyMVar mvar $ \ (a : r) -> return (r, a)
        action


-- | Deeply embedded DSL for (a subset of) TargetM ().
data IsolatedTargetM
    = Fail String
    | ReturnUnit
    | LogMessageLn String

    | IsolatedTargetM :>> IsolatedTargetM
  deriving Show

unwrap :: IsolatedTargetM -> TargetM ()
unwrap x = isolate $ case x of
    Fail s -> fail s
    ReturnUnit -> return ()
    LogMessageLn s -> logMessageLn s
    (a :>> b) -> unwrap a >> unwrap b

instance Monoid IsolatedTargetM where
    mempty = ReturnUnit
    mappend = (:>>)

instance EqProp IsolatedTargetM where
    a =-= b = morallyDubiousIOProperty $ do
      resultA <- hCapture [stdout, stderr] $ runTargetM $ unwrap a
      resultB <- hCapture [stdout, stderr] $ runTargetM $ unwrap b
      return $
          printTestCase (show resultA ++ "\n/=\n" ++ show resultB) $
          resultA == resultB

instance Arbitrary IsolatedTargetM where
    arbitrary = oneof $
        (Fail <$> arbitrary) :
        (return ReturnUnit) :
        (LogMessageLn <$> arbitrary) :
        ((:>>) <$> arbitrary <*> arbitrary) :
        []
    shrink (Fail s) = map Fail $ shrink s
    shrink ReturnUnit = []
    shrink (LogMessageLn s) = map LogMessageLn $ shrink s
    shrink (a :>> b) =
        [a' :>> b | a' <- shrink a] ++
        [a :>> b' | b' <- shrink b] ++
        [a, b]
