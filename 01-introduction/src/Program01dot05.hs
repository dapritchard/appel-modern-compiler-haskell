{- Transcription of Program 1.5 on page 9: Representation of straight-line
programs
-}
module Program01dot05 where

import Data.Map.Strict ( Map, empty, findWithDefault )
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

interpStm :: Stm -> Table -> IO Stm
interpStm _ _ = return (PrintStm [])

interpExp :: Exp -> Table -> IO (Integer, Table)
interpExp (IdExp v) t = return (findWithDefault 0 v t, t)
interpExp (NumExp x) t = return (x, t)
interpExp (OpExp e1 binop e2) t = do
  (x1, t1) <- interpExp e1 t
  (x2, t2) <- interpExp e2 t
  case binop of
    Plus -> return (x1 + x2, t2)
    Minus -> return (x1 - x2, t2)
    Times -> return (x1 * x2, t2)
    Div -> return (x1 `quot` x2, t2)
  return (0, empty)
interpExp (EseqExp s e) t = do
  interpStm s t
  interpExp e t


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
