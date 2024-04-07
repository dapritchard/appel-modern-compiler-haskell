module Exercise01dot01 where

import Data.List ( intercalate )

type Key = String

data Tree = Leaf | Tree Tree Key Tree
  deriving (Eq, Ord)

empty :: Tree
empty = Leaf

insert :: Key -> Tree -> Tree
insert key Leaf = Tree Leaf key Leaf
insert key (Tree l k r)
  | key < k   = Tree (insert key l) k r
  | key > k   = Tree l k (insert key r)
  | otherwise = Tree l k r


{- 1.1.a -----------------------------------------------------------------------

Implement a `member` function that returns `True` if the item is found, else
`False`
-}
member :: Key -> Tree -> Bool
member _ Leaf = False
member key (Tree l k r)
  | key < k   = member key l
  | key > k   = member key r
  | otherwise = True


{- 1.1.b -----------------------------------------------------------------------

Extend the program to include not just membership, but the mappping of keys to
bindings:
-}
data TreeKV a = LeafKV | TreeKV (TreeKV a) (Key, a) (TreeKV a)
  deriving (Eq, Ord, Show)

insertKV :: TreeKV a -> Key -> a -> TreeKV a
insertKV LeafKV key val = TreeKV LeafKV (key, val) LeafKV
insertKV (TreeKV l (k, v) r) key val
  | key < k   = TreeKV (insertKV l key val) (k, v) r
  | key > k   = TreeKV l (k, v) (insertKV r key val)
  | otherwise = TreeKV l (key, val) r

lookupKV :: TreeKV a -> Key -> Maybe a
lookupKV LeafKV _ = Nothing
lookupKV (TreeKV l (k, v) r) key
  | key < k   = lookupKV l key
  | key > k   = lookupKV r key
  | otherwise = Just v


{- 1.1.c -----------------------------------------------------------------------

These trees are not balanced; demonstrate the behavior on the following two
sequences of insertions:
-}

-- Utility function for drawing Trees
draw :: Tree -> [String]
draw Leaf = ["*"]
draw (Tree Leaf k Leaf) = [show k]
draw (Tree tl k tr) = [show k] ++ shiftl (draw tl) ++ shiftr (draw tr)
  where
    shiftl = zipWith (++) ("├─" : repeat "│ ")
    shiftr = zipWith (++) ("└─" : repeat " ")

-- Driver function for drawing Trees
instance Show Tree where
  show t = intercalate "\n" (draw t)

-- `print example1` to show a representation of the tree
example1 :: Tree
example1 = foldl (flip insert) Leaf ["t", "s", "p", "i", "p", "f", "b", "s", "t"]

-- `print example2` to show a representation of the tree
example2 :: Tree
example2 = foldl (flip insert) Leaf ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
