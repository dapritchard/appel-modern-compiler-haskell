module TestLexicalAnalysis (tests) where

import           TigerLexer              hiding ( main )
import           TigerLexerToo              qualified as Tiger2
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
  -- TODO: Tiger2 parser still needs work
  -- [ testsTiger, testsTigerToo, testsCompare ]

testsTiger :: TestTree
testsTiger = testGroup "TigerLexer" [ mkCompareLexTestTiger "test/testcases/test1.tig" tokensTestTiger1 ]


testsTigerToo :: TestTree
testsTigerToo = testGroup "TigerLexerToo" [ mkCompareLexTestTigerToo "test/testcases/test1.tig" tokensTestTigerToo1 ]

testsCompare :: TestTree 
testsCompare = testCaseSteps "TigerLexer v. TigerLexerToo in:" $ \step -> do
  let dir ="./test/testcases" 
  fs <- sort <$> listDirectory dir
  withCurrentDirectory dir $
    mapM_ (\f -> step f >> mkCompareSuccessfulLexersTest f) fs

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

tokensTestTigerToo1 :: [Tiger2.LexemeClass]
tokensTestTigerToo1 = map lexemeToLexemeClass tokensTestTiger1

{- Compare  lexers -}

mkCompareSuccessfulLexersTest :: FilePath -> Assertion
mkCompareSuccessfulLexersTest f = do
    s <- readFile f
    let expected = scanner s
    let actual = Tiger2.scanner $ Text.pack s
    -- Compare successful results. If one fails the test fails.
    let
      cmp (Left s) _ = assertFailure $ "TigerLexer.scanner failed with: " ++ s
      cmp _ (Left err) = 
        assertFailure $ "TigerLexerToo.scanner failed with: " ++ show err
      cmp (Right e) (Right a) = mapM_ (uncurry (@=?)) $
        zipWith (\l1 l2 -> (lexemeToLexemeClass l1, l2)) e a
 
    cmp expected actual

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


{- Test TigerLexerToo -}

mkCompareLexTestTigerToo :: FilePath -> [Tiger2.LexemeClass] -> TestTree
mkCompareLexTestTigerToo f lexemes = testCaseSteps ("Compare elements in: " ++ f)
  $ \step -> do
  actual <- Tiger2.scanner . Text.pack <$> readFile f
  case actual of
    Left err -> do
      step "Actual is Left"
      assertFailure "Lexing should succeed"
    Right lexemes' -> do
      let res = zip3 [0..] lexemes lexemes'
      mapM_ (\(i, e, a) -> step ("Elem " ++ show i) >> (e @=? a)) res

{- UTILS -}

lexemeToLexemeClass :: Lexeme -> Tiger2.LexemeClass
lexemeToLexemeClass (Lexeme _ l _ ) = convertLexemeClass l

-- NOTE: :(
convertLexemeClass :: LexemeClass -> Tiger2.LexemeClass
convertLexemeClass c = case c of
 EOF -> Tiger2.EOF
 ID x -> Tiger2.ID (Text.pack x)
 INT x -> Tiger2.INT x
 STRING x -> Tiger2.STRING (Text.pack x)
 COMMA -> Tiger2.COMMA
 COLON -> Tiger2.COLON
 SEMICOLON -> Tiger2.SEMICOLON
 LPAREN -> Tiger2.LPAREN
 RPAREN -> Tiger2.RPAREN
 LBRACK -> Tiger2.LBRACK
 RBRACK -> Tiger2.RBRACK
 LBRACE -> Tiger2.LBRACE
 RBRACE -> Tiger2.RBRACE
 DOT -> Tiger2.DOT
 PLUS -> Tiger2.PLUS
 MINUS -> Tiger2.MINUS
 TIMES -> Tiger2.TIMES
 DIVIDE -> Tiger2.DIVIDE
 EQ' -> Tiger2.EQ'
 NEQ -> Tiger2.NEQ
 LT' -> Tiger2.LT'
 LE -> Tiger2.LE
 GT' -> Tiger2.GT'
 GE -> Tiger2.GE
 AND -> Tiger2.AND
 OR -> Tiger2.OR
 UNARYMINUS -> Tiger2.UNARYMINUS
 ASSIGN -> Tiger2.ASSIGN
 IF -> Tiger2.IF
 THEN -> Tiger2.THEN
 ELSE -> Tiger2.ELSE
 WHILE -> Tiger2.WHILE
 FOR -> Tiger2.FOR
 TO -> Tiger2.TO
 DO -> Tiger2.DO
 END -> Tiger2.END
 BREAK -> Tiger2.BREAK
 OF -> Tiger2.OF
 LET -> Tiger2.LET
 IN -> Tiger2.IN
 ARRAY -> Tiger2.ARRAY
 NIL -> Tiger2.NIL
 FUNCTION -> Tiger2.FUNCTION
 VAR -> Tiger2.VAR
 TYPE -> Tiger2.TYPE
 EXCEPTION -> Tiger2.EXCEPTION
 TRY -> Tiger2.TRY
 HANDLE -> Tiger2.HANDLE
 RAISE -> Tiger2.RAISE


