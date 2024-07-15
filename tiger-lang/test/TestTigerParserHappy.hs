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
import TigerLexer (Lexeme(..), LexemeClass(INT))
import Symbol
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
    -- NOTE: undefined will not be evaluated
    --  see compareExp
    expectedVarVal = IntExp $ Lexeme undefined (INT 5) undefined
    expectedVar = VarDec' undefined undefined undefined expectedVarVal undefined
    expectedInExp = SeqExp undefined [VarExp (Var $ Symbol ("x", 0))]
    expected = Right $ LetExp undefined [VarDec expectedVar] expectedInExp

  compareTig actual expected

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

-- TODO: expand this as we add test cases.

-- | Modified version of @=? for tiger test cases.
compareTig :: (Eq e, Show e) => Either e Exp -> Either e Exp -> Assertion
compareTig (Right actual) (Right expected) = compareExp actual expected
compareTig actual expected = actual @=? expected

-- |  Modified version of @=? that compares
-- only on elements we care about for these tests.
-- Arguments are in order of actual expected.
compareExp :: Exp -> Exp -> Assertion
compareExp (LetExp _ decs exp) (LetExp _ decs' exp') = do
  mapM_ (uncurry compareDec) $ zip decs decs'
  compareExp exp exp'
compareExp (VarExp var) (VarExp var') = var @=? var'
compareExp (IntExp lexeme) (IntExp lexeme') = compareLexeme lexeme lexeme'
compareExp (SeqExp _ exps) (SeqExp _ exps') = mapM_ (uncurry compareExp) $ zip exps exps'

compareDec :: Dec -> Dec -> Assertion
compareDec (VarDec dec) (VarDec dec') = compareExp exp exp'
  where 
    exp = varDecInit dec
    exp' = varDecInit dec'
compareDec _ _ = assertFailure "Unsupported Dec variant"

-- | Compare on LexemeClass.
compareLexeme :: Lexeme -> Lexeme -> Assertion
compareLexeme (Lexeme _ c _) (Lexeme _ c' _) = c @=? c'
