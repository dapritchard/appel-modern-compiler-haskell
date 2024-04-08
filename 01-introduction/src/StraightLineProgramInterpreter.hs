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

import Control.Exception (Exception, throwIO)
import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT, get, gets)
import Control.Monad.State.Strict qualified as State
import Data.Map.Strict (Map, empty, insert)
import Data.Map.Strict qualified as M
import Data.Text (Text, pack)
import Data.Text.IO qualified as Text

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
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp

type Table = Map Id Int

-- Interpreter -----------------------------------------------------------------

data InterpErr
  = DivByZero
  | VariableNotFound Id
  deriving (Show)

instance Exception InterpErr

data IState = IState {table :: Table, output :: Text}
  deriving (Show)

type Interpreter = StateT IState (Either InterpErr)

-- | Interpret a new program starting with an empty environment.
interp :: Stm -> IO ()
interp s = do
  let init = IState {table = empty, output = ""}
      res = State.execStateT (interpStm s) init
  case res of
    Right (IState {output}) -> Text.putStr output
    Left err -> throwIO err

{- Interpret an expression with regards to a given environment using the types
suggested on page 11.
-}
interpExp :: Exp -> Interpreter Int
interpExp (IdExp nm) = tableLookup nm
interpExp (NumExp x) = pure x
interpExp (OpExp e1 op e2) = do
  x <- interpExp e1
  y <- interpExp e2
  calcBinop op x y
interpExp (EseqExp s e) = interpStm s >> interpExp e

{- Interpret a statment with regards to a given environment using the types
suggested on page 11. Output to print is accumulated in 'IState'.
-}
interpStm :: Stm -> Interpreter ()
interpStm (CompoundStm s1 s2) = interpStm s1 >> interpStm s2
interpStm (AssignStm nm e) = interpExp e >>= tableInsert nm
interpStm (PrintStm es) =
  -- We want a newline at the end
  -- but no extra space.
  let printExps [] = pure ()
      printExps [e] = interpExp e >>= expToOutput "\n"
      -- Concat results in output field of IState,
      -- separated by a space.
      printExps (e : es') =
        (interpExp e >>= expToOutput " ")
          >> printExps es'
   in printExps es

-- Helpers

calcBinop :: Binop -> Int -> Int -> Interpreter Int
calcBinop Plus x y = pure $ x + y
calcBinop Minus x y = pure $ x - y
calcBinop Times x y = pure $ x * y
calcBinop Div _ 0 = throwError DivByZero
calcBinop Div x y = pure $ x `div` y

-- | @M.'lookup'@, converting failures to find the given 'Id' into
-- 'VariableNotFound' errors.
tableLookup :: Id -> Interpreter Int
tableLookup nm = gets (M.lookup nm . table) >>= wrap
  where
    wrap Nothing = throwError (VariableNotFound nm)
    wrap (Just x) = pure x

-- | TODO: subtle difference with Appel's book
-- and M.lookup behavior: Former keeps the *first* definition?
tableInsert :: Id -> Int -> Interpreter ()
tableInsert nm x = State.modify' (modifyTable (M.insert nm x))

-- | 'tshow' the result of an 'Exp', which is an 'Int',
-- appending a separator, and adding it to 'output' of 'IState'.
expToOutput :: Text -> Int -> Interpreter ()
expToOutput sep x = State.modify' (modifyOutput (<> res))
  where
    res = tshow x <> sep

tshow :: (Show a) => a -> Text
tshow = pack . show

-- TODO: lenses help here.

-- | Modify the table element of IState.
modifyTable :: (Table -> Table) -> IState -> IState
modifyTable f t@(IState {table}) = t {table = f table}

-- | Modify the output element of IState.
modifyOutput :: (Text -> Text) -> IState -> IState
modifyOutput f t@(IState {output}) = t {output = f output}

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
