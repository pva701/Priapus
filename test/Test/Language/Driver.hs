module Test.Language.Driver where

import Universum

import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.Golden

import Language.Driver (evalProgramGolden)

examplesDir :: FilePath
examplesDir = "examples"

testEvalExample :: String -> TestTree
testEvalExample name =
    goldenVsFile testName gld outp $
    evalProgramGolden inp outp
  where
    testName = "File '" ++ name ++ "'"
    gld = examplesDir </> name <.> "gld"
    outp = examplesDir </> name <.> "out"
    inp = examplesDir </> name <.> "pr"

test_evalExamples :: TestTree
test_evalExamples = testGroup "Evaluating example programs"
    [ testEvalExample "example1"
    , testEvalExample "example2"
    , testEvalExample "example3"
    ]
