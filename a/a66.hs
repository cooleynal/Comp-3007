import Prelude hiding (tail)

-------------------------------------------------
-- Data types -----------------------------------
-------------------------------------------------

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

----------------------------------------------------------------------
-- Some useful functions for using `do` with `Maybe`. READ CAREFULLY.
----------------------------------------------------------------------

failed :: Maybe a -> Bool
failed Nothing = True
failed _ = False

failIf :: Bool -> Maybe ()
failIf True = Nothing
failIf False = Just ()

failUnless :: Bool -> Maybe ()
failUnless = failIf . not  -- Ensure the condition is true; otherwise fail.

headIs :: (a -> Bool) -> [a] -> Bool
headIs p (c : _) | p c = True
headIs _ _ = False

---------------------------------------------------------------
-- Some provided parsing building blocks. READ CAREFULLY.
---------------------------------------------------------------

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = \str -> case p1 str of
    Nothing -> p2 str
    Just val -> Just val

-- Succeeds iff there is nothing left to parse.
parseEnd :: Parser ()
parseEnd s = do
  failIf $ s /= ""
  return $ PR () ""

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789."

isLetter :: Char -> Bool
isLetter c = c `elem` "abcdefghijklmnopqrstuvwxyz"

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
-- Parser Implementations
------------------------------------------------------------------------------

parseExp :: Parser Exp
parseExp = parseApp2 +++ parseApp1 +++ parseConst +++ parseVar

parseConst :: Parser Exp
parseConst str = do
  PR digits rest1 <- parseDigits str
  let constValue = read digits :: Double
  return $ PR (Const constValue) rest1

parseVar :: Parser Exp
parseVar str = do
  PR name rest <- parseName str
  return $ PR (Var name) rest

parseOpName :: Parser String
parseOpName str = do
  PR name rest <- parseName str
  failUnless $ name `elem` ["plus", "times"]  -- Add other valid operator names as needed
  return $ PR name rest

-- Improved parseApp1 to handle nested applications
parseApp1 :: Parser Exp
parseApp1 str = do
  PR funcName rest <- parseName str
  PR _ rest' <- parseChar '(' rest
  PR arg rest'' <- parseExp rest'  -- Allow nested expressions as arguments
  PR _ finalRest <- parseChar ')' rest''
  return $ PR (App1 funcName arg) finalRest

-- Improved parseApp2 to handle nested applications
parseApp2 :: Parser Exp
parseApp2 str = do
  PR funcName rest <- parseOpName str  -- Parse the function name
  PR _ rest' <- parseChar '(' rest      -- Expect '('
  PR arg1 rest'' <- parseExp rest'      -- Parse the first argument
  PR _ rest''' <- parseChar ',' rest''  -- Expect ','
  PR arg2 finalRest <- parseExp rest''' -- Parse the second argument
  PR _ finalRest' <- parseChar ')' finalRest  -- Expect ')'
  return $ PR (App2 funcName arg1 arg2) finalRest'

parseDef :: Parser Def
parseDef str = do
  PR name rest <- parseName str
  PR _ rest' <- parseChar '=' rest
  PR exp rest'' <- parseExp rest'
  parseEnd rest''  -- Ensure there's nothing left to parse
  return $ PR (Def name exp) rest''

printDefs :: [Def] -> IO ()
printDefs [] = return ()
printDefs (def : defs) = do
  putStrLn (show def)
  printDefs defs

-- Read in a file named program.txt where each line is a string representing a
-- Def. Parse each line and use printDefs to printout the parsed Defs.
main :: IO ()
main = do
  contents <- readFile "program.txt"
  putStrLn "Original contents:"
  putStrLn contents

  let trimmedContents = filter (/= ' ') contents
  putStrLn "Trimmed contents:"
  putStrLn trimmedContents

  let linesOfFile = lines trimmedContents
  putStrLn "trimmed and arrayed"
  print linesOfFile

  putStrLn "Parsed definitions:"
  processLines linesOfFile

processLines :: [String] -> IO ()
processLines [] = return ()
processLines (line:lines) = do
  let result = parseDef line
  case result of
    Just (PR def _) -> putStrLn (show def)
    Nothing         -> putStrLn ("Parsing failed for line: " ++ line)

  processLines lines
