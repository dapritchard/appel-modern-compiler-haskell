module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import TestLexicalAnalysis (lexerAlexTests)
import TestTigerParserHappy (parserHappyTests)

main :: IO ()
main = defaultMain $ testGroup "all tests" [lexerAlexTests, parserHappyTests]
