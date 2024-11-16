{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemeSyntax where

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
import Data.List
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Maybe
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

------------------------------------------------
-- COMP 3007 stuff (was in Assignment 9 file) --
------------------------------------------------

-- ASTs for Scheme expressions
data Exp
  = Atom String
  | List [Exp]
  | Number Int
  | String String
  | Nil
  | Bool Bool
  deriving (Eq, Show)

data Program = Program
  { programDefs :: [Exp]
  , programExp :: Exp
  }
  deriving (Show, Eq)

parseExp :: String -> Exp
parseExp str = case readExpr (T.pack str) of
  Right v ->
    let e = convert v
     in assert isWFExp ("parseExp: expression not well formed: " ++ unparseExp e) e
  Left e -> error $ show e

parseProgram :: String -> Program
parseProgram str =
  let prog@(Program defs e) = parseRawProgram str
      msg = "parsePprogram not well-formed: " ++ concatMap unparseExp defs ++ unparseExp e
   in assert isWFProgram msg (Program defs e)

myunsnoc :: [a] -> Maybe ([a], a)
myunsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

parseRawProgram :: String -> Program
parseRawProgram str =
  let List es' = parseExp $ "(list " ++ str ++ ")"
      es = tl es'
   in if null es
        then
          error "parseProgram: empty program"
        else
          let (defs, e) = fromJust (myunsnoc es)
           in Program defs e

isWFExp :: Exp -> Bool
isWFExp (List (Atom _ : es)) | length es <= 4 = True
isWFExp (List (_ : _)) = False
isWFExp _ = True

isWFProgram :: Program -> Bool
isWFProgram (Program defs e) =
  all isDef defs && not (isDef e)

isDef :: Exp -> Bool
isDef (List [Atom "define", List (Atom f : vars), e]) =
  all isAtom vars && noDuplicates vars
isDef _ = False

isAtom :: Exp -> Bool
isAtom (Atom _) = True
isAtom _ = False

isList :: Exp -> Bool
isList (List _) = True
isList _ = False

isBool :: Exp -> Bool
isBool (Bool _) = True
isBool _ = False

isNumber :: Exp -> Bool
isNumber (Number _) = True
isNumber _ = False

isString :: Exp -> Bool
isString (String _) = True
isString _ = False

unparseExp :: Exp -> String
unparseExp (Atom str) = str
unparseExp (List es) = "(" ++ unwords (map unparseExp es) ++ ")"
unparseExp (Number n) = show n
unparseExp (String str) = show str
unparseExp Nil = "'()"
unparseExp (Bool True) = "#t"
unparseExp (Bool False) = "#f"

unparseProgram :: Program -> String
unparseProgram (Program defs e) = concatMap unparseExp defs ++ unparseExp e

-- Convert the Syntax module's ASTs to Exps.
instance Convert LispVal Exp where
  convert (LVAtom txt) = Atom (T.unpack txt)
  convert (LVList es) = List $ map convert es
  convert (LVNumber n) = Number (fromInteger n)
  convert (LVString txt) = String (T.unpack txt)
  convert LVNil = Nil
  convert (LVBool b) = Bool b

instance Convert Exp Int where
  convert (Number x) = x
  convert e = error $ "can't convert to number: " ++ show e

instance Convert Exp Bool where
  convert (Bool x) = x
  convert e = error $ "can't convert to boolean: " ++ show e

instance Convert Exp String where
  convert (String x) = x
  convert e = error $ "can't convert to string: " ++ show e

class Convert a b where
  convert :: a -> b

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates l = length l == length (nub l)

failingIf :: Bool -> Maybe a -> Maybe a
failingIf b x = if b then Nothing else x

failingUnless :: Bool -> Maybe a -> Maybe a
failingUnless b x = if b then x else Nothing

-- Check if an object satisfies a predicate, returning the object if it does,
-- else raising an error with the given message.
assert :: (a -> Bool) -> String -> a -> a
assert p _ x | p x = x
assert _ msg x = error msg

hd :: [a] -> Maybe a
hd (a : _) = Just a
hd _ = Nothing

tl :: [a] -> [a]
tl (x : l) = l
tl [] = []

--
-----------------------------------------
-- External stuff -----------------------
-- --------------------------------------

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
  = LVAtom T.Text
  | LVList [LispVal]
  | LVNumber Integer
  | LVString T.Text
  | LVNil
  | LVBool Bool
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
    (LVAtom atom) -> atom
    (LVString txt) -> T.concat ["\"", txt, "\""]
    (LVNumber num) -> T.pack $ show num
    (LVBool True) -> "#t"
    (LVBool False) -> "#f"
    LVNil -> "'()"
    (LVList contents) -> T.concat ["(", unwordsList contents, ")"]

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
            $> LVBool True
              <|> char 'f'
            $> LVBool False
              <|> char 'b'
            *> (LVNumber <$> intRadix (2, oneOf "01"))
              <|> char 'o'
            *> (LVNumber <$> intRadix (8, octDigit))
              <|> char 'd'
            *> (LVNumber <$> intRadix (10, digit))
              <|> char 'x'
            *> (LVNumber <$> intRadix (16, hexDigit))
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
    <|> LVNil
    <$ nil
      <|> LVNumber
    <$> try (sign <*> decimal)
      <|> LVAtom
    <$> identifier
      <|> LVString
    <$> textLiteral
      <|> _Quote
    <$> quoted lispVal
      <|> LVList
    <$> parens manyLispVal

manyLispVal :: Parser [LispVal]
manyLispVal = lispVal `sepBy` whitespace

_Quote :: LispVal -> LispVal
_Quote x = LVList [LVAtom "quote", x]

contents :: Parser a -> Parser a
contents p = whitespace *> lexeme p <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents lispVal) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents (LVList <$> manyLispVal))
