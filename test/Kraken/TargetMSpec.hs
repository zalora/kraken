
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
import           Kraken.Util


spec :: Spec
spec = do

    describe "abort" $ do

        it "reports aborts as Lefts" $ do
            runTargetM (abort "foo" :: TargetM ()) `shouldReturn` Left [(Nothing, "foo")]

        it "writes aborts to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM (abort "foo")
            output `shouldContain` "foo"

        it "does not execute subsequent actions" $ do
            output <- capture_ $ runTargetM (abort "foo" >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` (not . ("bar" `isInfixOf`))

    describe "isolate" $ do

        it "does execute subsequent actions in case of aborts" $ do
            output <- capture_ $ runTargetM (isolate (abort "foo") >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` ("bar" `isInfixOf`)

        it "does execute subsequent actions in case of Exceptions" $ do
            output <- capture_ $ runTargetM (isolate (error "foo") >> liftIO (putStrLn "bar"))
            output `shouldSatisfy` ("bar" `isInfixOf`)

        it "does collect errors" $ do
            result <- runTargetM (isolate (abort "foo") >> (abort "bar" :: TargetM ()))
            result `shouldBe` Left [(Nothing, "foo"), (Nothing, "bar")]

        it "prints aborts to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM $
                isolate (abort "foo") >>
                liftIO (hPutStrLn stderr "bar")
            output `shouldBe` "<no target>:\n    foo\nbar\n"

        it "prints exceptions to stderr" $ do
            output <- hCapture_ [stderr] $ runTargetM (isolate (error "foo"))
            output `shouldContain` "foo"

        it "converts exceptions to Lefts" $ do
            runTargetM (isolate (error "foo")) `shouldReturn`
                Left [(Nothing, "foo")]

        testBatch $ monoid (error "proxy" :: IsolatedTargetM)


-- | Deeply embedded DSL for (a subset of) TargetM ().
data IsolatedTargetM
    = Abort String
    | ReturnUnit
    | LogMessageLn String

    | IsolatedTargetM :>> IsolatedTargetM
  deriving Show

unwrap :: IsolatedTargetM -> TargetM ()
unwrap x = isolate $ case x of
    Abort s -> abort s
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
        (Abort <$> arbitrary) :
        (return ReturnUnit) :
        (LogMessageLn <$> arbitrary) :
        ((:>>) <$> arbitrary <*> arbitrary) :
        []
    shrink (Abort s) = map Abort $ shrink s
    shrink ReturnUnit = []
    shrink (LogMessageLn s) = map LogMessageLn $ shrink s
    shrink (a :>> b) =
        [a' :>> b | a' <- shrink a] ++
        [a :>> b' | b' <- shrink b] ++
        [a, b]
