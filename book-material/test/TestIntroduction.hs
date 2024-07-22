module Main (main) where

import           Exercise01dot01
import           Test.Tasty                     ( TestTree
                                                , defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@=?)
                                                , (@?)
                                                , testCase
                                                )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Tests of BinarySearchTrees"
  [ testsInsert
  ]

testsInsert :: TestTree
testsInsert = testGroup
  "Tests of insert"
  [ testCase "Add to empty tree" $ insert "m" Leaf @=? treeSingle
  , testCase "Add to left tree" $ insert "f" treeSingle @=? treeLeft
  , testCase "Add to right tree" $ insert "v" treeSingle @=? treeRight
  , testCase "Member exists" $ insert "m" treeSingle @=? treeSingle
  ]

testsMember :: TestTree
testsMember = testGroup
  "Tests of member"
  [ testCase "Check left tree" $ member "f" treeLeft @? "\"f\" is in the tree"
  , testCase "Check center tree" $ member "m" treeLeft @? "\"m\" is in the tree"
  , testCase "Check right tree" $ member "v" treeRight @? "\"v\" is in the tree"
  , testCase "Check leaf" $ member "m" Leaf @? "\"m\" is not in the tree"
  , testCase "Member not in tree" $ member "z" treeLeft @? "\"z\" is not in the tree"
  ]

treeSingle :: Tree
treeSingle = Tree Leaf "m" Leaf

treeLeft :: Tree
treeLeft = Tree (Tree Leaf "f" Leaf) "m" Leaf

treeRight :: Tree
treeRight = Tree Leaf "m" (Tree Leaf "v" Leaf)
