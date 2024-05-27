module TestTigerParserHappy (parserHappyTests) where

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
import TigerParserHappy (parse)
import AST (Exp, Exp' (NilExp), Program (..))

testDirLoc :: String
testDirLoc = "./test/testcases/"

parserHappyTests :: TestTree
parserHappyTests = testGroup "tests for parser built using Happy"
                             [ -- testBasicLetTig
                             -- , testTest1Tig
                             ]

testBasicLetTig :: TestTree
testBasicLetTig =
  testCase
    "basic-let.tig"
    assertBasicLetTig

assertBasicLetTig :: Assertion
assertBasicLetTig = do
  fileContents <- readFile (testDirLoc ++ "basic-let.tig")
  let actual = lexAndParse fileContents
      -- This isn't expected to be the correct value of @ast@, it's just trying to
      -- provoke the exception that is occuring
      expected = Right $ NilExp (AlexPn 0 0 0) :: Either String Exp
  actual @=? expected

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

lexAndParse :: String -> Either String Exp
lexAndParse fileContents =
  runAlex fileContents parse
