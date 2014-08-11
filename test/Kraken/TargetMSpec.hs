
module Kraken.TargetMSpec where


import           Control.Applicative
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


spec :: Spec
spec = do

    describe "exception handling" $ do
        it "converts exceptions to Lefts" $ do
            runTargetM (error "foo" :: TargetM ()) `shouldReturn`
                Left [(Nothing, "foo")]

        it "prints exceptions to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM (error "foo" :: TargetM ())
            output `shouldContain` "foo"

    describe "abort" $ do

        it "reports aborts as Lefts" $ do
            runTargetM (abort "foo" :: TargetM ()) `shouldReturn` Left [(Nothing, "foo")]

        it "writes aborts to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM (abort "foo")
            output `shouldContain` "foo"

        it "does not execute subsequent actions" $ do
            output <- capture_ $ runTargetM (abort "foo" >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` (not . ("bar" `isInfixOf`))

    describe ">>!" $ do

        it "does execute actions after abort" $ do
            output <- capture_ $ runTargetM (abort "foo" >>! liftIO (putStrLn "bar"))
            output `shouldSatisfy` ("bar" `isInfixOf`)

        it "does collect errors" $ do
            result <- runTargetM (abort "foo" >>! abort "bar")
            result `shouldBe` Left [(Nothing, "foo"), (Nothing, "bar")]

        it "also writes aborts to stderr at the time of execution" $ do
            output <- hCapture_ [stderr] $ runTargetM $
                abort "foo" >>!
                liftIO (hPutStrLn stderr "bar")
            output `shouldBe` "<no target>:\n    foo\nbar\n"

        testBatch $ monoid (error "proxy" :: WrappedTargetM)


-- | Deeply embedded DSL for (a subset of) TargetM ().
data WrappedTargetM
    = Abort String
    | ReturnUnit
    | LogMessageLn String

    | WrappedTargetM :>>! WrappedTargetM
  deriving Show

unwrap :: WrappedTargetM -> TargetM ()
unwrap x = do
  case x of
    Abort s -> abort s
    ReturnUnit -> return ()
    LogMessageLn s -> logMessageLn s
    (a :>>! b) -> unwrap a >>! unwrap b

instance Monoid WrappedTargetM where
    mempty = ReturnUnit
    mappend = (:>>!)

instance EqProp WrappedTargetM where
    a =-= b = morallyDubiousIOProperty $ do
      resultA <- hCapture [stdout, stderr] $ runTargetM $ unwrap a
      resultB <- hCapture [stdout, stderr] $ runTargetM $ unwrap b
      return $
          printTestCase (show resultA ++ "\n/=\n" ++ show resultB) $
          resultA == resultB

instance Arbitrary WrappedTargetM where
    arbitrary = oneof $
        (Abort <$> arbitrary) :
        (return ReturnUnit) :
        (LogMessageLn <$> arbitrary) :
        ((:>>!) <$> arbitrary <*> arbitrary) :
        []
    shrink (Abort s) = map Abort $ shrink s
    shrink ReturnUnit = []
    shrink (LogMessageLn s) = map LogMessageLn $ shrink s
    shrink (a :>>! b) =
        [a' :>>! b | a' <- shrink a] ++
        [a :>>! b' | b' <- shrink b] ++
        [a, b]
