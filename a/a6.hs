-- Assignment 6 Due Sunday Oct 13 23:59

-- You will need almost no Prelude functions to do this assignment. You would
-- find `elem` useful, and you will need `putStrLn` and `readFile`. You will
-- also need to know about the "case" statement, which is very similar to the
-- pattern matching you already know. This file also imports `trace`, which is
-- handy sometimes for debugging. Google for more info.

-- You'll be implementing a "parser". We define data types to represent the
-- expressions of a very simple programming language. A program is a list of
-- "definitions". Here is an example program:
--   f=plus(times(3.3,23.4),0.0)
--   g=x
--   h=f(f(g(x,y)),h(z) )
-- Each line is a *name*, followed by an "=", followed by an *expression*.
-- An expression is one of the following:
-- - a number with a decimal point
-- - a (function) name "applied" to one or two argument expressions that are
-- surrounded by parentheses and separated (when there are two arguments) by
-- commas.
-- - an "if" statement which is like the application case above except the
-- "function" name is "if" and there are three arguments.
-- - a name that is not applied as a function, i.e. it is not followed by "(".
--
-- A program cannot contain any blanks or tab characters. Each line must be a
-- single definition. A name can only contain lower case letters.




module A6 where
import Debug.Trace (trace)
import Prelude hiding (head, tail)

-------------------------------------------------
-- Data types -----------------------------------
-------------------------------------------------

---- only lower case letters allowed
type Name = String

-- Expressions
data Exp
  = Const Double
  | Var String
  | If Exp Exp Exp
  | App1 Name Exp
  | App2 Name Exp Exp
  deriving (Show, Eq)

-- Definitions
data Def
  = Def Name Exp
  deriving (Show, Eq)

-- A "parser" takes a string, finds a prefix of it that corresponds to an
-- element of a, then, if successful, returns that element together with the
-- remainder of the string.
type Parser a = String -> Maybe (PR a)

-- parser result
data PR a = PR a String
  deriving (Show, Eq)

reservedWords = words "if"

----------------------------------------------------------------------
-- Some useful functions for using `do` with `Maybe`. READ CAREFULLY.
----------------------------------------------------------------------

failed :: Maybe a -> Bool
failed Nothing = True
failed _ = False

deJust :: [Maybe a] -> Maybe [a]
deJust [] = Just []
deJust (x : xs) = do
  s <- x
  ss <- deJust xs
  return (s : ss)

failIf :: Bool -> Maybe ()
failIf True = Nothing
failIf False = Just ()

failUnless :: Bool -> Maybe ()
failUnless = failIf . not

headIs :: (a -> Bool) -> [a] -> Bool
headIs p (c : l) | p c = True
headIs _ _ = False

---------------------------------------------------------------
-- Some provided parsing building blocks. READ CAREFULLY.
---------------------------------------------------------------

-- Apply p1 to the tiven string. If it gives a result, return it, otherwise
-- return the rsult of apply p2.
(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 =
  \str -> case p1 str of
    Nothing -> p2 str
    Just val -> Just val

-- Succeeds iff there is nothing left to parse.
parseEnd :: Parser ()
parseEnd s = do
  failIf $ s /= ""
  return $ PR () ""

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789."

-- isDigit :: Char -> Bool
-- isDigit x = x `elem` "0123456789"

isLetter :: Char -> Bool
isLetter c = c `elem` "abcdefghigklmnopqrstuvwxyz"

-- Fail if the first character in the given string is not c, otherwise the
-- result is c and the rest of the string
parseChar :: Char -> Parser Char
parseChar c str = do
  (c', rest) <- maybeCons str
  failUnless $ c == c'
  return $ PR c rest

maybeCons :: [a] -> Maybe (a, [a])
maybeCons (x : l) = Just (x, l)
maybeCons _ = Nothing

-- collect p is a parser that returns the longest prefix of the given string
-- such that every character in it satisfies p. The result is the prefix and the
-- rest of the string.
collect :: (Char -> Bool) -> Parser String
collect p str = do
  if headIs p str
    then do
      (c, rest) <- maybeCons str
      PR collected rest' <- collect p rest
      return $ PR (c : collected) rest'
    else do
      return $ PR [] str

-- get the longest prefix of letters
parseName :: Parser Name
parseName = collect isLetter

-- get the longest prefix of digits
parseDigits :: Parser String
parseDigits = collect isDigit

------------------------------------------------------------------------------
-- Code for you to write. All your parser definitions should start as shown,
-- i.e. with an argument (say `str`) for the string input to the parser, a
-- right-hand side that starts with `do`, and all the rest of the right-hand
-- side should be lines in the `do`.
------------------------------------------------------------------------------

-- parseExp :: Parser Exp
-- parseExp = parseConst +++ parseVar +++ parseIf +++ parseApp1 +++ parseApp2

-- parseConst :: Parser Exp
-- parseConst str = do
--   undefined

-- parseVar :: Parser Exp
-- parseVar str = do
--   undefined

-- parseOpName :: Parser String
-- parseOpName str = do
--   undefined

-- parseIf :: Parser Exp
-- parseIf str = do
--   undefined

-- parseApp2 :: Parser Exp
-- parseApp2 str = do
--   undefined

-- parseApp1 :: Parser Exp
-- parseApp1 str = do
--   undefined

-- parseDef :: Parser Def
-- parseDef str = do
--   undefined

-- printDefs :: [Def] -> IO ()
-- printDefs [] = return ()
-- printDefs (def : defs) = do
--   putStrLn (show def)
--   printDefs defs

-- -- Read in a file named program.txt where each line is a string representing a
-- -- Def. Parse each line and use printDefs to printout the parsed Defs.
-- main = do
--   undefined
