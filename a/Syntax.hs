{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import Control.Exception (Exception)
import Control.Monad (mzero)
import Control.Monad.Reader (
  MonadIO,
  MonadReader,
  ReaderT (ReaderT),
 )
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Text qualified as T
import Text.Parsec (
  ParseError,
  SourceName,
  char,
  digit,
  eof,
  hexDigit,
  letter,
  many1,
  octDigit,
  oneOf,
  parse,
  sepBy,
  string,
  try,
  (<?>),
  (<|>),
 )
import Text.Parsec.Language qualified as Lang
import Text.Parsec.Text (Parser)
import Text.Parsec.Token qualified as Tok

type ValCtx = Map.Map T.Text LispVal

type FnCtx = Map.Map T.Text LispVal

data EnvCtx = EnvCtx
  { env :: ValCtx
  , fenv :: FnCtx
  }
  deriving (Eq, Show)

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Nil
  | Bool Bool
  deriving (Eq, Show)

-- TODO make a pretty printer
data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String -- from show anyway
  | IOError T.Text

instance Exception LispException

instance Show LispException where
  show = T.unpack . showError

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String txt) -> T.concat ["\"", txt, "\""]
    (Number num) -> T.pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "'()"
    (List contents) -> T.concat ["(", unwordsList contents, ")"]

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list

showError :: LispException -> T.Text
showError err =
  case err of
    (IOError txt) -> T.concat ["Error reading file: ", txt]
    (NumArgs int args) -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
    (LengthOfList txt int) -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    (ExpectedList txt) -> T.concat ["Error Expected List in funciton ", txt]
    (TypeMismatch txt val) -> T.concat ["Error Type Mismatch: ", txt, showVal val]
    (BadSpecialForm txt) -> T.concat ["Error Bad Special Form: ", txt]
    (NotFunction val) -> T.concat ["Error Not a Function: ", showVal val]
    (UnboundVar txt) -> T.concat ["Error Unbound Variable: ", txt]
    (PError str) -> T.concat ["Parser Error, expression cannot evaluate: ", T.pack str]
    (Default val) -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style =
  Lang.emptyDef
    { Tok.commentStart = "{-"
    , Tok.commentEnd = "-}"
    , Tok.commentLine = ";"
    , Tok.opStart = mzero
    , Tok.opLetter = mzero
    , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"
    , Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
    }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
 where
  specialIdentifier :: Parser String
  specialIdentifier =
    lexeme $
      try $
        string "-" <|> string "+" <|> string "..."

{- | The @Radix@ type consists of a base integer (e.g. @10@) and a parser for
digits in that base (e.g. @digit@).
-}
type Radix = (Integer, Parser Char)

{- | Parse an integer, given a radix as output by @radix@.
Copied from Text.Parsec.Token
-}
numberWithRadix :: Radix -> Parser Integer
numberWithRadix (base, baseDigit) = do
  digits <- many1 baseDigit
  let n = foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

decimal :: Parser Integer
decimal = Tok.decimal lexer

{- | Parse a sign, return either @id@ or @negate@ based on the sign parsed.
Copied from Text.Parsec.Token
-}
sign :: Parser (Integer -> Integer)
sign =
  char '-'
    $> negate
      <|> char '+'
    $> id
      <|> return id

intRadix :: Radix -> Parser Integer
intRadix r = sign <*> numberWithRadix r

textLiteral :: Parser T.Text
textLiteral = T.pack <$> Tok.stringLiteral lexer

nil :: Parser ()
nil = try (char '\'' *> string "()") *> return () <?> "nil"

hashVal :: Parser LispVal
hashVal =
  lexeme $
    char '#'
      *> ( char 't'
            $> Bool True
              <|> char 'f'
            $> Bool False
              <|> char 'b'
            *> (Number <$> intRadix (2, oneOf "01"))
              <|> char 'o'
            *> (Number <$> intRadix (8, octDigit))
              <|> char 'd'
            *> (Number <$> intRadix (10, digit))
              <|> char 'x'
            *> (Number <$> intRadix (16, hexDigit))
              <|> oneOf "ei"
            *> fail "Unsupported: exactness"
              <|> char '('
            *> fail "Unsupported: vector"
              <|> char '\\'
            *> fail "Unsupported: char"
         )

lispVal :: Parser LispVal
lispVal =
  hashVal
    <|> Nil
    <$ nil
      <|> Number
    <$> try (sign <*> decimal)
      <|> Atom
    <$> identifier
      <|> String
    <$> textLiteral
      <|> _Quote
    <$> quoted lispVal
      <|> List
    <$> parens manyLispVal

manyLispVal :: Parser [LispVal]
manyLispVal = lispVal `sepBy` whitespace

_Quote :: LispVal -> LispVal
_Quote x = List [Atom "quote", x]

contents :: Parser a -> Parser a
contents p = whitespace *> lexeme p <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents lispVal) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents (List <$> manyLispVal))
