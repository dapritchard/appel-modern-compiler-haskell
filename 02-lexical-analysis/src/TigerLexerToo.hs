-- | Tiger lexer, largely following along
-- with https://markkarpov.com/tutorial/megaparsec.html#lexing
module TigerLexerToo where

import Control.Applicative (Alternative, asum, empty, many, (<|>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P.Char
import Text.Megaparsec.Char.Lexer qualified as P.Lexer

type Parser = P.Parsec Void Text

type Error = P.ParseErrorBundle Text Void

-- | TODO:
scanner :: Text -> Either Error [LexemeClass]
scanner = P.parse lexemes ""

-- | Example to run in GHCi.
example :: Text
example = "\"ax\"(1203) \"abc\""

exampleNoParse :: Text
exampleNoParse = "sumg@rB@ge"

-- Lexer components

-- | Whitespace and comment parser.
space :: Parser ()
space =
  P.Lexer.space
    P.Char.space1
    empty
    (P.Lexer.skipBlockComment "/*" "*/")

-- | Combinator for defining lexemes.
mkLexeme :: Parser a -> Parser a
mkLexeme = P.Lexer.lexeme space

-- | TODO:
symbol :: Text -> Parser Text
symbol = mkLexeme . P.Lexer.symbol space

-- TODO: is "_" allowed?
-- | [a-z][a-zA-Z0-9]*
identifier :: Parser Text
identifier = mkLexeme p
  where
    p = T.cons <$> P.Char.lowerChar <*>
         (mconcat <$> many P.Char.alphaNumChar)


-- | Integer lexer.
integer :: Parser Int
integer = mkLexeme P.Lexer.decimal

-- NOTE: Taken directly from the blog.

-- | TODO:
stringLiteral :: Parser Text
stringLiteral = mkLexeme $
  P.Char.char '\"'
    *> manyTill P.Lexer.charLiteral (P.Char.char '\"')

manyTill :: (Alternative m) => m Char -> m Char -> m Text
manyTill p end = go
  where
    go = ("" <$ end) <|> (T.cons <$> p <*> go)

-- Lexemes

-- TODO: unclear whether this is correct sequencing
lexemes :: Parser LexemeClass
lexemes = many l <* eof
  where 
    l = asum 
          [ident
          , int
          , string
          , punctuation
          , math
          , assign
          , keywords
          ]

eof,
  ident,
  int,
  string,
  punctuation,
  math,
  controlFlow :: Parser LexemeClass
eof = EOF <$ P.eof
ident = ID <$> identifier
int = INT <$> integer
string = STRING <$> stringLiteral
punctuation =
  asum
    [ COMMA <$ symbol ",",
      COLON <$ symbol ":",
      SEMICOLON <$ symbol ";",
      LPAREN <$ symbol "(",
      RPAREN <$ symbol ")",
      LBRACK <$ symbol "[",
      RBRACK <$ symbol "]",
      LBRACE <$ symbol "{",
      RBRACE <$ symbol "}",
      DOT <$ symbol "."
    ]
math =
  asum
    [ PLUS <$ symbol "\+",
      MINUS <$ symbol "\-",
      TIMES <$ symbol "\*",
      DIVIDE <$ symbol "\/",
      EQ' <$ symbol "\=",
      NEQ <$ symbol "\<\>",
      LT' <$ symbol "\<",
      LE <$ symbol "\<\=",
      GT' <$ symbol "\>",
      GE <$ symbol "\>\=",
      AND <$ symbol "&",
      OR <$ symbol "\|"
    ]
assign = ASSIGN <$ symbol ":\="
keywords =
  asum
    [ TYPE <$ symbol "type",
      VAR <$ symbol "var",
      FUNCTION <$ symbol "function",
      BREAK <$ symbol "break",
      OF <$ symbol "of",
      END <$ symbol "end",
      IN <$ symbol "in",
      NIL <$ symbol "nil",
      LET <$ symbol "let",
      DO <$ symbol "do",
      TO <$ symbol "to",
      FOR <$ symbol "for",
      WHILE <$ symbol "while",
      ELSE <$ symbol "else",
      THEN <$ symbol "then",
      IF <$ symbol "if",
      ARRAY <$ symbol "array",
      EXCEPTION <$ symbol "exception",
      HANDLE <$ symbol "handle",
      TRY <$ symbol "try",
      RAISE <$ symbol "raise"
    ]


-- | See TigerLexer.x
data LexemeClass
  = EOF
  | ID Text
  | INT Int
  | STRING Text
  -- punctuation
  | COMMA
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | DOT
  -- math
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ'
  | NEQ
  | LT'
  | LE
  | GT'
  | GE
  | AND
  | OR
  -- TODO: What is this?
  | UNARYMINUS
  -- assign
  | ASSIGN
  -- keywords
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | TO
  | DO
  | END
  | BREAK
  | OF
  | LET
  | IN
  | ARRAY
  | NIL
  | FUNCTION
  | VAR
  | TYPE
  | EXCEPTION
  | TRY
  | HANDLE
  | RAISE
  deriving (Show, Eq)
