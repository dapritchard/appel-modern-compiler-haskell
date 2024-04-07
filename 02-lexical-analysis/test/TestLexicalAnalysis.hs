module Main (main) where

import           TigerLexer              hiding ( main )
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , (@?)
                                                , assertFailure
                                                , testCase
                                                )

-- Test the lexing of the test1.tig file
main :: IO ()
main = do
  s <- readFile "test/testcases/test1.tig"
  let sr = scanner s
  let tests = testGroup "test1.tig" [compareLex sr (Right tokens)]
  defaultMain tests

-- The expected values of the lexed test1.tig contents
tokens :: [Lexeme]
tokens =
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

type Scan = Either String [Lexeme]

-- Test for equality on a element-by-element basis
compareLex :: Scan -> Scan -> TestTree
compareLex (Left x) (Left y) = testCase "Compare elements" $ x @=? y
compareLex (Left _) (Right _) = testCase "Compare elements" (assertFailure "Left vs. Right")
compareLex (Right _) (Left _) = testCase "Compare elements" (assertFailure "Right vs. Left")
compareLex (Right x) (Right y)
  | length x /= length y = testCase "Compare elements" (assertFailure "Differing lengths")
  | null x               = testGroup "Compare elements" []
  | otherwise            = testGroup "Compare elements" $ map test (zip3 [0..] (init x) (init y))
 where
  test (n, a, b) = testCase ("Elem" ++ show n) $ a @=? b
