{- Transcription of Program 1.5 on page 9: Representation of straight-line
programs
-}
module Program01dot05 where

type Id = String

data Binop = Plus | Minus | Times | Div

data Stm =
    CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp =
    IdExp
  | NumExp Integer
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp
