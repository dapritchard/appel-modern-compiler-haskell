module TestTigerParserHappy (tests) where

import TigerLexer hiding (main)
import Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , (@?)
                                                , assertFailure
                                                , testCase
                                                , testCaseSteps
                                                , Assertion
                                                )
import Data.Either (fromRight)
import Parser (parse)
import Language
-- import AST (Exp, Exp' (NilExp), Program (..))

tests :: TestTree
tests = testGroup "tests for parser built using Happy"
                             [ testBasicLetTig
                             -- , testTest1Tig
                             ]

testDirLoc :: String
testDirLoc = "./test/testcases/"

basicLetTig :: String
basicLetTig = "basic-let.tig"

testBasicLetTig :: TestTree
testBasicLetTig =
  testCase
    basicLetTig
    assertBasicLetTig

assertBasicLetTig :: Assertion
assertBasicLetTig = do
  fileContents <- readFile (testDirLoc ++ basicLetTig)
  let 
    actual = lexAndParse fileContents

      -- This isn't expected to be the correct value of @ast@, it's just trying to
      -- provoke the exception that is occuring
  actual @=? Left ""

testTest1Tig :: TestTree
testTest1Tig = do
  testCase
    "test1.tig"
    assertTest1Tig

assertTest1Tig :: Assertion
assertTest1Tig = do
  fileContents <- readFile (testDirLoc ++ "test1.tig")
  let actual = lexAndParse fileContents
      -- This isn't expected to be the correct value of @ast@, it's just trying to
      -- provoke the exception that is occuring
      expected = Right $ NilExp (AlexPn 0 0 0) :: Either String Exp
  actual @=? expected

{- UTILS -}

lexAndParse :: String -> Either String Exp
lexAndParse fileContents =
  runAlex fileContents parse
