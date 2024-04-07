{- Transcription of Program 1.5 on page 9: Representation of straight-line
programs
-}
module Program01dot05 where

import Data.Map.Strict ( Map, empty )
import Data.Text ( Text )


-- Types -----------------------------------------------------------------------

type Id = String

data Binop = Plus | Minus | Times | Div

data Stm =
    CompoundStm !Stm !Stm
  | AssignStm !Id !Exp
  | PrintStm ![Exp]

data Exp =
    IdExp !Text
  | NumExp !Integer
  | OpExp !Exp !Binop !Exp
  | EseqExp !Stm !Exp

type Table = Map Text Integer


-- Interpreter -----------------------------------------------------------------

-- interp :: Stm -> IO ()

interpStm :: Stm -> Table -> Stm
interpStm _ _ = PrintStm []

interpExp :: Exp -> Table -> (Integer, Table)
interpExp _ _ = (0, empty)

-- Data ------------------------------------------------------------------------

prog :: Stm
prog = CompoundStm topLevelLHS topLevelRHS

topLevelLHS :: Stm
topLevelLHS = AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3))

topLevelRHS :: Stm
topLevelRHS =
  CompoundStm
    (AssignStm
      "b"
      (EseqExp
        (PrintStm [
            IdExp "a"
          , OpExp (IdExp "a") Minus (NumExp 1)
          ]
        )
        (OpExp (NumExp 10) Times (IdExp "a"))
      )
    )
    (PrintStm [IdExp "b"])
