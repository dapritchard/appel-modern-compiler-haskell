module Main where


import Test.Tasty                     ( TestTree
                                      , defaultMain
                                      , testGroup
                                      )

import TestTigerParserHappy qualified as Parser
import TestLexicalAnalysis qualified as Lexer

main :: IO () 
main = defaultMain 
  $ testGroup "tiger-lexer tests"
      [Lexer.tests, Parser.tests]

