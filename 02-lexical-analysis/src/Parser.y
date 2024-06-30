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
  : let Decs in SeqExps end                  { LetExp (tokenToPos $1) (reverse $2) (SeqExp (tokenToPos $1) (reverse $4)) }
  | break                                    { BreakExp (tokenToPos $1) }
  | nil                                      { NilExp (tokenToPos $1) }
  | for Id ':=' Exp to Exp do Exp %prec 'do' { ForExp (tokenToPos $1) $2 $4 $6 $8 False }
  | while Exp do Exp %prec 'do'              { WhileExp (tokenToPos $1) $2 $4 }
  | Var ':=' Exp                             { AssignExp (tokenToPos $2) $1 $3 }
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

{- For 'Sequencing', page 516 -}
SeqExps :: {[Exp]}
  : {- empty -}     { [] }
  | Exp             { [$1] }
  | SeqExps ';' Exp { $3 : $1 }

Decs :: {[Dec]}
  : {- empty -} { [] }
  | Decs Dec    { $2 : $1 }

Dec :: {Dec}
  : TyDec  { TyDec $1 }
  | VarDec  { VarDec $1 }
  | FunDec { FunDec $1 }

TyDec :: {TyDec'}
  : type Id '=' Ty { TyDec' (tokenToPos $1) $2 $4 }

VarDec :: {VarDec'}
  : var Id ':' Id ':=' Exp { VarDec' (tokenToPos $1) $2 (Just $4) $6 False }
  | var Id ':=' Exp        { VarDec' (tokenToPos $1) $2 Nothing $4 False }

FunDec :: {FunDec'}
  : function Id '(' Tyfields ')' ':' Id '=' Exp { FunDec' (tokenToPos $1) $2 (reverse $4) (Just $7) $9 }
  | function Id '(' Tyfields ')' '=' Exp        { FunDec' (tokenToPos $1) $2 (reverse $4) Nothing $7 }

{- For function application parameter inputs ('Function call'), page 517 -}
Exps :: {[Exp]}
  : {- empty -}  { [] }
  | Exp          { [$1] }
  | Exps ',' Exp { $3 : $1 }

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

Ty :: {Ty}
  : Id               { IdTy $1 }
  | '{' Tyfields '}' { FieldsTy (tokenToPos $1) (reverse $2) }
  | array of Id      { ArrayOfTy (tokenToPos $1) $3 }

Tyfields :: {[TyField]}
  : {- empty -}            { [] }
  | Id ':' Id              { [TyField (tokenToPos $2) $1 $3 False] }
  | Tyfields ',' Id ':' Id { TyField (tokenToPos $2) $3 $5 False : $1 }

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
