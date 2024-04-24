-- | Tiger lexer, largely following along
-- with https://markkarpov.com/tutorial/megaparsec.html#lexing
module TigerLexerToo where

import Control.Monad (void)
import Control.Applicative (Alternative, asum, empty, some, many, (<|>))
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
    --  TODO:
    -- Line comments are effectively block comments
    (P.Lexer.skipBlockComment "/*" "*/")
    (P.Lexer.skipBlockComment "/*" "*/")

-- | Combinator for defining lexemes.
mkLexeme :: Parser a -> Parser a
mkLexeme = P.Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = P.Lexer.symbol space

-- TODO: need a better way to do exact
-- match, since we might not want to require
-- whitespace after.

-- | Match exactly the keyword 's'.
-- 'try' used to allow backtracking.
keyword :: Text -> Parser Text
keyword s = P.Lexer.lexeme space p
  where p = P.try $ P.Char.string s <* P.Char.space1

-- TODO: might need to be different
symbolExact :: Text -> Parser Text
symbolExact = keyword

char :: Char -> Parser Char
char = mkLexeme . P.Char.char

-- | [a-zA-Z][_a-zA-Z0-9]*
identifier :: Parser Text
identifier = mkLexeme p
  where
    underscore = char '_'
    p = T.cons <$> P.Char.letterChar <*>
      (T.pack <$> many (P.Char.alphaNumChar <|> underscore))


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

-- TODO: Need to set precedence here

-- | See note in 'P.Char.Lexer' about the convention of
-- needing to call 'space' before any other lexemes to catch
-- whitespace or comments at the beginning of the file.
lexemes :: Parser [LexemeClass]
lexemes = space *> many l <* eof
  where 
    l = keywords <|> asum 
          [ ident
          , int
          , string
          , punctuation
          , math
          , assign
          ]

eof,
  ident,
  int,
  string,
  punctuation,
  math,
  keywords :: Parser LexemeClass
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
    [ PLUS <$ symbol "+",
      MINUS <$ symbol "-",
      TIMES <$ symbol "*",
      DIVIDE <$ symbol "/",
      EQ' <$ symbol "=",
      NEQ <$ symbol "<>",
      LT' <$ symbol "<",
      LE <$ keyword "<=",
      GT' <$ symbol ">",
      GE <$ keyword ">=",
      AND <$ symbol "&",
      OR <$ symbol "|"
    ]
-- Needed to avoid partial match of :
assign = ASSIGN <$ keyword ":="
keywords =
  asum
    [ TYPE <$ keyword "type",
      VAR <$ keyword "var",
      FUNCTION <$ keyword "function",
      BREAK <$ keyword "break",
      OF <$ keyword "of",
      END <$ keyword "end",
      IN <$ keyword "in",
      NIL <$ keyword "nil",
      LET <$ keyword "let",
      DO <$ keyword "do",
      TO <$ keyword "to",
      FOR <$ keyword "for",
      WHILE <$ keyword "while",
      ELSE <$ keyword "else",
      THEN <$ keyword "then",
      IF <$ keyword "if",
      ARRAY <$ keyword "array",
      EXCEPTION <$ keyword "exception",
      HANDLE <$ keyword "handle",
      TRY <$ keyword "try",
      RAISE <$ keyword "raise"
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
