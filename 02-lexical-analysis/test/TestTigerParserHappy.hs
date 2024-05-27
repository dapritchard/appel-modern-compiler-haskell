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

-- testBasicLetTig :: TestTree
-- testBasicLetTig = do
--   testCase
--     "basic-let.tig"
--     assertBasicLetTig

-- assertBasicLetTig :: Assertion
-- assertBasicLetTig = do
--   tokens <- scanner <$> readFile (testDirLoc ++ "basic-let.tig")
--   let ast = parse (fromRight [] tokens)
--   -- -- This isn't expected to be the correct value of @ast@, it's just trying to
--   -- -- provoke the exception that is occuring
--   -- ast @=? Program (NilExp (AlexPn 0 0 0))
--   ast @=? NilExp (AlexPn 0 0 0)

-- testTest1Tig :: TestTree
-- testTest1Tig = do
--   testCase
--     "test1.tig"
--     assertTest1Tig

-- assertTest1Tig :: Assertion
-- assertTest1Tig = do
--   tokens <- scanner <$> readFile (testDirLoc ++ "test1.tig")
--   let ast = parse (fromRight [] tokens)
--   -- -- This isn't expected to be the correct value of @ast@, it's just trying to
--   -- -- provoke the exception that is occuring
--   -- ast @=? Program (NilExp (AlexPn 0 0 0))
--   ast @=? NilExp (AlexPn 0 0 0)
