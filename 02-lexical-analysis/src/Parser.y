{
{-# OPTIONS -w #-}
module Parser where

import Language
import Symbol
import TigerLexer hiding (Pos)

}

-- The expression language used here comes straight from the happy
-- documentation with virtually no changes (open, so TokenOB/TokenCB were
-- changed to TokenLParen/TokenRParen

%name parse
%tokentype { Lexeme }
%monad { Alex }
%lexer { lexer } {  Lexeme _ EOF _ }
-- Without this we get a type error
%error { parseError }

%token
  ID            { Lexeme _ (ID _) _ }
  INT           { Lexeme _ (INT _) _ }
  STRING        { Lexeme _ (STRING _) _ }
  ','           { Lexeme _ COMMA _ }
  ':'           { Lexeme _ COLON _ }
  ';'           { Lexeme _ SEMICOLON _ }
  '('           { Lexeme _ LPAREN _ }
  ')'           { Lexeme _ RPAREN _ }
  '{'           { Lexeme _ LBRACK _ }
  '}'           { Lexeme _ RBRACK _ }
  '['           { Lexeme _ LBRACE _ }
  ']'           { Lexeme _ RBRACE _ }
  '.'           { Lexeme _ DOT _ }
  '+'           { Lexeme _ PLUS _ }
  '-'           { Lexeme _ MINUS _ }
  '*'           { Lexeme _ TIMES _ }
  '/'           { Lexeme _ DIVIDE _ }
  '='           { Lexeme _ EQ' _ }
  '<>'          { Lexeme _ NEQ _ }
  '<'           { Lexeme _ LT' _ }
  '<='          { Lexeme _ LE _ }
  '>'           { Lexeme _ GT' _ }
  '>='          { Lexeme _ GE _ }
  '&'           { Lexeme _ AND _ }
  '|'           { Lexeme _ OR _ }
  ':='          { Lexeme _ ASSIGN _ }
  array         { Lexeme _ ARRAY _ }
  if            { Lexeme _ IF _ }
  then          { Lexeme _ THEN _ }
  else          { Lexeme _ ELSE _ }
  while         { Lexeme _ WHILE _ }
  for           { Lexeme _ FOR _ }
  to            { Lexeme _ TO _ }
  do            { Lexeme _ DO _ }
  let           { Lexeme _ LET _ }
  in            { Lexeme _ IN _ }
  end           { Lexeme _ END _ }
  of            { Lexeme _ OF _ }
  break         { Lexeme _ BREAK _ }
  nil           { Lexeme _ NIL _ }
  function      { Lexeme _ FUNCTION _ }
  var           { Lexeme _ VAR _ }
  type          { Lexeme _ TYPE _ }
{-UNARYMINUS    { Lexeme _ UNARYMINUS _ }
  EXCEPTION     { Lexeme _ EXCEPTION _ }
  TRY           { Lexeme _ TRY _ }
  HANDLE        { Lexeme _ HANDLE _ }
  RAISE         { Lexeme _ RAISE _ }
-}

%nonassoc 'function' 'var' 'type' 'then' 'do' 'of' ':='
%nonassoc 'else'
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '>=' '<='
%left '+' '-'
%left '*' '/'
%left NEG

%%

{-
Exp   : let var '=' Exp in Exp  { Let "" $4 $6 }
      | Exp1                    { Exp1 $1 }
-}

Exp :: {Exp}
  : Var ':=' Exp                             { AssignExp (tokenToPos $2) $1 $3 }
  | Id '[' Exp ']' of Exp %prec 'do'         { ArrayExp (tokenToPos $2) $1 $3 $6 }
  | Id '{' RecordFields '}'                  { RecordExp (tokenToPos $2) $1 (reverse $3) }
  | Id '(' Exps ')'                          { FuncallExp (tokenToPos $2) $1 (reverse $3) }
  | Var                                      { VarExp $1 }
  | Exp '&' Exp                              { OpExp (tokenToPos $2) AndOp $1 $3 }
  | Exp '|' Exp                              { OpExp (tokenToPos $2) OrOp $1 $3 }
  | Exp '=' Exp                              { OpExp (tokenToPos $2) EqOp $1 $3 }
  | Exp '<>' Exp                             { OpExp (tokenToPos $2) NeqOp $1 $3 }
  | Exp '>' Exp                              { OpExp (tokenToPos $2) GtOp $1 $3 }
  | Exp '<' Exp                              { OpExp (tokenToPos $2) LtOp $1 $3 }
  | Exp '>=' Exp                             { OpExp (tokenToPos $2) GteOp $1 $3 }
  | Exp '<=' Exp                             { OpExp (tokenToPos $2) LteOp $1 $3 }
  | Exp '-' Exp                              { OpExp (tokenToPos $2) SubOp $1 $3 }
  | Exp '+' Exp                              { OpExp (tokenToPos $2) AddOp $1 $3 }
  | Exp '*' Exp                              { OpExp (tokenToPos $2) MulOp $1 $3 }
  | Exp '/' Exp                              { OpExp (tokenToPos $2) DivOp $1 $3 }
  | '-' Exp %prec NEG                        { OpExp (tokenToPos $1) SubOp zeroExp $2 }
  | INT                                      { IntExp $1 }
  | STRING                                   { StringExp $1 }

Var :: {Var}
  : Id              { Var $1 }
  | VarTail         { $1 }     {- Uses VarTail to handle shift/reduce conflict with ArrayExp -}

VarTail :: {Var}
  : Id '.' Id           { RecField (tokenToPos $2) (Var $1) $3 }
  | VarTail '.' Id      { RecField (tokenToPos $2) $1 $3 }
  | Id '[' Exp ']'      { ArraySub (tokenToPos $2) (Var $1) $3 }
  | VarTail '[' Exp ']' { ArraySub (tokenToPos $2) $1 $3 }

Id :: {Symbol}
  {- FIXME -}
  : ID { Symbol (tokenToString $1, 0) }

RecordFields :: {[(Pos, Symbol, Exp)]}
  : {- empty -}                 { [] }
  | Id '=' Exp                  { [(tokenToPos $2, $1, $3)] }
  | RecordFields ',' Id '=' Exp { ((tokenToPos $2), $3, $5) : $1 }

{-
Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { IntExp $1 }
      | var                     { Var "" }
      | '(' Exp ')'             { Brack $2 }
-}

{
-- TODO: Could also use `tokPosn`
tokenToPos :: Lexeme -> AlexPosn
tokenToPos (Lexeme p _ _) = p

tokenToString :: Lexeme -> String
tokenToString (Lexeme _ _ Nothing) = ""
tokenToString (Lexeme _ _ (Just x)) = x

-- lexwrap :: (Token -> Alex a) -> Alex a
-- lexwrap = (alexMonadScan' >>=)

parseError :: Lexeme -> a
parseError (Lexeme p _ t) =
  error $ "Parse error at position: " ++ show p ++ " with token: " ++ show t

-- parseExp :: FilePath -> String -> Either String Exp
-- parseExp = runAlex' parse

parseExp :: String
parseExp = ""

zeroExp :: Exp
zeroExp = IntExp zeroLexeme

zeroLexeme :: Lexeme
zeroLexeme = Lexeme (AlexPn 0 0 0) (INT 0) Nothing
}
