module TestLexicalAnalysis (tests) where

import           TigerLexer              hiding ( main )
import           Test.Tasty                     ( TestTree
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
import Data.Text qualified as Text
import System.Directory (listDirectory, withCurrentDirectory)
import Data.List (sort)

tests :: TestTree
tests = testGroup "Lexer tests" [testsTiger]

testsTiger :: TestTree
testsTiger = testGroup "TigerLexer" [ mkCompareLexTestTiger "test/testcases/test1.tig" tokensTestTiger1 ]

-- The expected values of the lexed test1.tig contents
tokensTestTiger1 :: [Lexeme]
tokensTestTiger1 =
  [ Lexeme (AlexPn 42 2 1) LET (Just "let")
  , Lexeme (AlexPn 47 3 9) TYPE (Just "type")
  , Lexeme (AlexPn 53 3 15) (ID "arrtype") (Just "arrtype")
  , Lexeme (AlexPn 61 3 23) EQ' (Just "=")
  , Lexeme (AlexPn 63 3 25) ARRAY (Just "array")
  , Lexeme (AlexPn 69 3 31) OF (Just "of")
  , Lexeme (AlexPn 72 3 34) (ID "int") (Just "int")
  , Lexeme (AlexPn 77 4 9) VAR (Just "var")
  , Lexeme (AlexPn 81 4 13) (ID "arr1") (Just "arr1")
  , Lexeme (AlexPn 85 4 17) COLON (Just ":")
  , Lexeme (AlexPn 86 4 18) (ID "arrtype") (Just "arrtype")
  , Lexeme (AlexPn 94 4 26) ASSIGN (Just ":=")
  , Lexeme (AlexPn 97 4 29) (ID "arrtype") (Just "arrtype")
  , Lexeme (AlexPn 105 4 37) LBRACK (Just "[")
  , Lexeme (AlexPn 106 4 38) (INT 10) (Just "10")
  , Lexeme (AlexPn 108 4 40) RBRACK (Just "]")
  , Lexeme (AlexPn 110 4 42) OF (Just "of")
  , Lexeme (AlexPn 113 4 45) (INT 0) (Just "0")
  , Lexeme (AlexPn 115 5 1) IN (Just "in")
  , Lexeme (AlexPn 119 6 9) (ID "arr1") (Just "arr1")
  , Lexeme (AlexPn 124 7 1) END (Just "end")
  , Lexeme undefined EOF Nothing
  ]

{- Test TigerLexer -}
type Scan = Either String [Lexeme]

-- | Read the file and compare to the expected value given.
mkCompareLexTestTiger :: FilePath -> [Lexeme] -> TestTree
mkCompareLexTestTiger f lexemes = testCaseSteps ("Compare elements in: " ++ f)
  $ \step -> do
  actual <- scanner <$> readFile f
  let expected = Right lexemes
  mapM_ (\(msg, t) -> step msg >> t) $ compareLex expected actual

-- Test for equality on a element-by-element basis
compareLex :: Scan -> Scan -> [(String, IO ())]
compareLex (Left x) (Left y) = [("", x @=? y)]
compareLex (Left _) (Right _) = [("", assertFailure "Left vs. Right")]
compareLex (Right _) (Left _) = [("", assertFailure "Right vs. Left")]
compareLex (Right x) (Right y)
  | length x /= length y = [("", assertFailure "Differing lengths")]
  | otherwise            = map test (zip3 [0..] (init x) (init y))
 where
  test (n, a, b) = (("Elem" ++ show n), a @=? b)
