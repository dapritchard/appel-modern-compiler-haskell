{-# LANGUAGE StrictData #-}

{- | Symbol table defined to match functional interface
in program 5.6 of the tiger book. To keep track of the invariant
(see section "Symbols in the Tiger compiler") that different strings
must have different integers, this implementation
-}
module Symbol (
  Symbol (..),
) where

import Data.STRef.Strict
import GHC.Generics (Generic)
import GHC.ST (ST, runST)

data Symbol = Symbol {symbolName :: String, symbolId :: Int}
  deriving (Generic, Show, Eq, Ord)
