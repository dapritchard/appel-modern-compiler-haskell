module Language where

import Symbol (Symbol)
import TigerLexer (AlexPosn, Lexeme)

type Pos = AlexPosn

-- This example comes straight from the happy documentation

-- data Exp
--       = Let String Exp Exp
--       | Exp1 Exp1
--       deriving (Eq, Show)

data Exp
  = VarExp Var
  | NilExp Pos
  | SeqExp Pos [Exp]
  | IntExp Lexeme
  | StringExp Lexeme
  | OpExp Pos Op Exp Exp
  | FuncallExp Pos Symbol [Exp]
  | RecordExp Pos Symbol [(Pos, Symbol, Exp)]
  | ArrayExp Pos Symbol Exp Exp
  | AssignExp Pos Var Exp
  | IfExp Pos Exp Exp (Maybe Exp)
  | WhileExp Pos Exp Exp
  | ForExp Pos Symbol Exp Exp Exp Bool
  | BreakExp Pos
  | LetExp Pos [Dec] Exp
  deriving (Eq, Show)

data Op
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | EqOp
  | NeqOp
  | GtOp
  | LtOp
  | GteOp
  | LteOp
  | AndOp
  | OrOp
  deriving (Show, Eq)

data Var
  = Var Symbol
  | RecField Pos Var Symbol
  | ArraySub Pos Var Exp
  deriving (Show, Eq)

data Dec
  = TyDec TyDec'
  | VarDec VarDec'
  | FunDec FunDec'
  deriving (Show, Eq)

data TyDec' = TyDec'
  { typeDecPos :: Pos
  , typeDecName :: Symbol
  , typeDecTy :: Ty
  }
  deriving (Show, Eq)

data TyField = TyField Pos Symbol Symbol Bool
  deriving (Show, Eq)

data Ty
  = IdTy Symbol
  | FieldsTy Pos [TyField]
  | ArrayOfTy Pos Symbol
  deriving (Show, Eq)

data VarDec' = VarDec'
  { varDecPos :: Pos
  , varDecName :: Symbol
  , varDecTy :: Maybe Symbol
  , varDecInit :: Exp
  , varDecEscapes :: Bool
  }
  deriving (Show, Eq)

data FunDec' = FunDec'
  { funDecPos :: Pos
  , funDecName :: Symbol
  , funDecParams :: [TyField]
  , funDecResult :: Maybe Symbol
  , funDecBody :: Exp
  }
  deriving (Show, Eq)

-- data Exp'
--   = IntExp Lexeme

-- data Exp1
--       = Plus Exp1 Term
--       | Minus Exp1 Term
--       | Term Term
--       deriving (Eq, Show)

-- data Term
--       = Times Term Factor
--       | Div Term Factor
--       | Factor Factor
--       deriving (Eq, Show)

-- data Factor
--       = Int Int
--       | Var String
--       | Brack Exp
--       deriving (Eq, Show)

-- eval :: [(String,Int)] -> Exp -> Int
-- eval p (Let var e1 e2) = eval ((var, eval p e1): p) e2
-- eval p (Exp1 e)        = evalExp1 p e
--   where
--   evalExp1 p' (Plus  e' t) = evalExp1 p' e' + evalTerm p' t
--   evalExp1 p' (Minus e' t) = evalExp1 p' e' + evalTerm p' t
--   evalExp1 p' (Term  t)    = evalTerm p' t

--   evalTerm p' (Times t f) = evalTerm p' t * evalFactor p' f
--   evalTerm p' (Div   t f) = evalTerm p' t `div` evalFactor p' f
--   evalTerm p' (Factor f)  = evalFactor p' f

--   evalFactor _  (Int i)    = i
--   evalFactor p' (Var s)    = case lookup s p' of
--                              Nothing -> error "free variable"
--                              Just i  -> i
--   evalFactor p' (Brack e') = eval p' e'
