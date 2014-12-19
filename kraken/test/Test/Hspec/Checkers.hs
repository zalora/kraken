
module Test.Hspec.Checkers (testBatch) where


import           Test.Hspec
import           Test.QuickCheck.Checkers


testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
    foldr (>>) (return ()) (map convertTest tests)

convertTest :: Test -> Spec
convertTest = uncurry it
