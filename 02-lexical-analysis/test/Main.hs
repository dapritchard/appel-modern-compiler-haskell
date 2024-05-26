module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import TestLexicalAnalysis (lexerAlexTests)

main :: IO ()
main = defaultMain $ testGroup "all tests" [lexerAlexTests]
