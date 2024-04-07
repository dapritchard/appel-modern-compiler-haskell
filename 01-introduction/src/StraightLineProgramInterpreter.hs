{- This is an implementation of the interpreter described in the Section 1.5 on
pages 10-12.

Run `interp prog` in GHCi to try it out.
-}
{-# LANGUAGE StrictData #-}

module StraightLineProgramInterpreter
  ( Binop (..),
    Exp (..),
    Id (..),
    Stm (..),
    Table,
    interp,
    prog,
  )
where

import Data.Map.Strict (Map, empty, findWithDefault, insert)
import Data.Text (Text)

-- Types -----------------------------------------------------------------------

{- These types are mostly a transciption of the types presented in Figure 1.4 on
page 9. The `Table` type was left open to interpretation and `Map` was chosen
for this.
-}

type Id = Text

data Binop = Plus | Minus | Times | Div

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp
  = IdExp Text
  | NumExp Integer
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp

type Table = Map Text Integer

-- Interpreter -----------------------------------------------------------------

-- Interpret a new program starting with an empty environment
interp :: Stm -> IO ()
interp s = undefined

{- Interpret an expression with regards to a given environment using the types
suggested on page 11. Two defects of this interpreter component are that:
1. If a program refers to a variable name that doesn't exist then a value of `0`
is used
2. If the program divides by 0 then the interpreter will throw an exception
-}
interpExp :: Exp -> Table -> IO (Integer, Table)
interpExp = undefined

{- Interpret a statment with regards to a given environment using the types
suggested on page 11. The `PrintStm` case cheats a little bit by printing a
trailing space at the end of the line.
-}
interpStm :: Stm -> Table -> IO Table
interpStm = undefined

-- Data ------------------------------------------------------------------------

-- This is the example program on page 10
prog :: Stm
prog = CompoundStm topLevelLHS topLevelRHS

-- The left-hand side of the top level of the program on page 10
topLevelLHS :: Stm
topLevelLHS = AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3))

-- The right-hand side of the top level of the program on page 10
topLevelRHS :: Stm
topLevelRHS =
  CompoundStm
    ( AssignStm
        "b"
        ( EseqExp
            ( PrintStm
                [ IdExp "a",
                  OpExp (IdExp "a") Minus (NumExp 1)
                ]
            )
            (OpExp (NumExp 10) Times (IdExp "a"))
        )
    )
    (PrintStm [IdExp "b"])
