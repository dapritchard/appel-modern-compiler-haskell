{- Transcription of Program 1.5 on page 9: Representation of straight-line
programs
-}
module Program01dot05 where

import Data.Map.Strict ( Map, empty, findWithDefault, insert )
import Data.Text ( Text )


-- Types -----------------------------------------------------------------------

type Id = Text

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

interp :: Stm -> IO ()
interp s = do
  interpStm s empty
  return ()

interpStm :: Stm -> Table -> IO Table
interpStm (CompoundStm s1 s2) t = do
  newT <- interpStm s1 t
  interpStm s2 newT
interpStm (AssignStm id e) t = do
  (v, newT) <- interpExp e t
  return (insert id v newT)
interpStm (PrintStm []) t = do
  putStrLn ""
  return t
interpStm (PrintStm (e:es)) t = do
  (v, newT) <- interpExp e t
  putStr (show v)
  interpStm (PrintStm es) newT

interpExp :: Exp -> Table -> IO (Integer, Table)
interpExp (IdExp v) t = return (findWithDefault 0 v t, t)
interpExp (NumExp x) t = return (x, t)
interpExp (OpExp e1 binop e2) t = do
  (x1, newT1) <- interpExp e1 t
  (x2, newT2) <- interpExp e2 newT1
  case binop of
    Plus -> return (x1 + x2, newT2)
    Minus -> return (x1 - x2, newT2)
    Times -> return (x1 * x2, newT2)
    Div -> return (x1 `quot` x2, newT2)
  return (0, empty)
interpExp (EseqExp s e) t = do
  newT <- interpStm s t
  interpExp e newT


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
