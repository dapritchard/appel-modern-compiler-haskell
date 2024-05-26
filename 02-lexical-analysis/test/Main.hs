module Main (main) where

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

-- Test the lexing of the test1.tig file
main :: IO ()
main = defaultMain $ testGroup "" []
