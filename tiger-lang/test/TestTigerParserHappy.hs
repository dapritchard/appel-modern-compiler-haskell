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
                             , testTest1Tig
                             ]


{- TEST CASES -}

testDirLoc :: String
testDirLoc = "./test/testcases/"

basicLetTig :: FilePath
basicLetTig = testDirLoc ++ "basic-let.tig"

testBasicLetTig :: TestTree
testBasicLetTig =
  testCase
    basicLetTig
    assertBasicLetTig

assertBasicLetTig :: Assertion
assertBasicLetTig = do
  actual <- lexAndParse <$> readFile basicLetTig
  let
    -- TODO: Symbol ids are as expected? If so creates a problem for the equality
    -- instance of Symbol.
    expectedVarVal = IntExp $ mkTestableLexeme (INT 5)
    expectedVar = mkTestableVarDec' (Symbol ("x", 0)) (Just $ Symbol ("int", 0)) expectedVarVal
    expectedInExp = mkTestableSeqExp [VarExp (Var $ Symbol ("x", 0))]
    expected = Right $ mkTestableLetExp [VarDec expectedVar] expectedInExp

  compareTig actual expected

test1Tig :: FilePath
test1Tig = testDirLoc ++ "test1.tig"

testTest1Tig :: TestTree
testTest1Tig = do
  testCase
    test1Tig
    assertTest1Tig

assertTest1Tig :: Assertion
assertTest1Tig = do
  actual <- lexAndParse <$> readFile test1Tig
  let
    arrtype = Symbol ("arrtype", 0)
    expectedArrayOfTy = ArrayOfTy undefined (Symbol ("int", 0))
    expectedTyDec = mkTestableTyDec' arrtype expectedArrayOfTy
    expectedVarArrLenVal = IntExp $ mkTestableLexeme (INT 10)
    expectedVarArrVal = IntExp $ mkTestableLexeme (INT 0)
    expectedVarVal = ArrayExp undefined arrtype expectedVarArrLenVal expectedVarArrVal
    expectedVar = mkTestableVarDec' (Symbol ("arr1", 0)) (Just arrtype) expectedVarVal
    expectedInExp = mkTestableSeqExp [VarExp (Var $ Symbol ("arr1", 0))]
    expected = Right $ mkTestableLetExp [TyDec expectedTyDec, VarDec expectedVar] expectedInExp
  
  compareTig actual expected

{- UTILS -}

lexAndParse :: String -> Either String Exp
lexAndParse fileContents =
  runAlex fileContents parse

-- | Constructor filling in only the tested elements of `LetExp`.
-- See 'compareExp'.
mkTestableLetExp :: [Dec] -> Exp -> Exp
mkTestableLetExp decs inExp = LetExp undefined decs inExp

mkTestableSeqExp :: [Exp] -> Exp
mkTestableSeqExp exps = SeqExp undefined exps

mkTestableVarDec' :: Symbol -> Maybe Symbol -> Exp -> VarDec'
mkTestableVarDec' nm ty val = 
  VarDec' { varDecPos = undefined,
            varDecName = nm,
            varDecTy = ty,
            varDecInit = val,
            varDecEscapes = undefined
  }

mkTestableTyDec' :: Symbol -> Ty -> TyDec'
mkTestableTyDec' nm ty = TyDec' undefined nm ty

mkTestableLexeme :: LexemeClass -> Lexeme
mkTestableLexeme cl = Lexeme undefined cl undefined

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
compareExp (ArrayExp _ ty lenExp valExp) (ArrayExp _ ty' lenExp' valExp') = do
  ty @=? ty'
  compareExp lenExp lenExp'
  compareExp valExp valExp'

-- | Compare Dec, with argument order actual expected
compareDec :: Dec -> Dec -> Assertion
compareDec (VarDec dec) (VarDec dec') = do
  nm @=? nm'
  ty @=? ty'
  compareExp exp exp'
  where 
    (VarDec' _ nm ty exp _ ) = dec
    (VarDec'  _ nm' ty' exp' _ ) = dec'
compareDec (TyDec dec) (TyDec dec') = do 
  nm @=? nm'
  compareTy ty ty'
  where
    (TyDec' _ nm ty) = dec
    (TyDec' _ nm' ty') = dec'
compareDec (FunDec _) _ = assertFailure "Unsupported Dec variant: FunDec"
compareDec _ _ = assertFailure "Mismatched Dec variants"

-- | Compare Ty, with order actual expected
compareTy :: Ty -> Ty -> Assertion
compareTy (IdTy ty) (IdTy ty') = ty @=? ty'
compareTy (ArrayOfTy _ ty) (ArrayOfTy _ ty') = ty @=? ty'

-- | Compare on LexemeClass.
compareLexeme :: Lexeme -> Lexeme -> Assertion
compareLexeme (Lexeme _ c _) (Lexeme _ c' _) = c @=? c'
