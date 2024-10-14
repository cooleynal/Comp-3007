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




-- module A6 where
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
  isDigit x = x `elem` "0123456789"

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

  -- import A6

  parseExp :: Parser Exp
  parseExp = parseConst +++ parseVar +++ parseIf +++ parseApp1 +++ parseApp2

  parseConst :: Parser Exp
  parseConst str = do
      PR intPart restAfterDigits <- parseDigits str
      PR _ restAfterDecimal <- parseChar '.' restAfterDigits
      PR fracPart restAfterFrac <- parseDigits restAfterDecimal
      let numberString = intPart ++ "." ++ fracPart
      let number = read numberString :: Double
      return $ PR (Const number) restAfterFrac

  parseVar :: Parser Exp
  parseVar str = do
      PR name rest <- parseName str
      failIf (headIs (== '(') rest)
      return (PR (Var name) rest)


  parseOpName :: Parser String
  parseOpName str = do
      PR name rest <- parseName str
      failIf (headIs (/= '(') rest)
      return $ PR name rest


  parseIf :: Parser Exp
  parseIf str = do
      PR opName rest1 <- parseOpName str
      -- failIf (opName /= "if")
      PR _ rest2 <- parseChar '(' rest1
      PR cond rest3 <- parseExp rest2
      -- trace ("if condition: " ++ show cond) $ return ()
      PR _ rest4 <- parseChar ',' rest3
      PR thenExp rest5 <- parseExp rest4
      PR _ rest6 <- parseChar ',' rest5
      PR elseExp rest7 <- parseExp rest6
      PR _ rest8 <- parseChar ')' rest7
      return $ PR (If cond thenExp elseExp) rest8


  parseApp2 :: Parser Exp
  parseApp2 str = do
      PR funcName rest1 <- parseOpName str
      -- failIf (funcName == "if")
      PR _ rest2 <- parseChar '(' rest1
      PR arg1 rest3 <- parseExp rest2
      PR _ rest4 <- parseChar ',' rest3
      PR arg2 rest5 <- parseExp rest4
      PR _ rest6 <- parseChar ')' rest5
      return $ PR (App2 funcName arg1 arg2) rest6


  parseApp1 :: Parser Exp
  parseApp1 str = do
      PR funcName rest1 <- parseName str
      -- failIf (funcName == "if")
      PR _ rest2 <- parseChar '(' rest1
      PR arg rest3 <- parseExp rest2
      PR _ rest4 <- parseChar ')' rest3
      return $ PR (App1 funcName arg) rest4

  parseDef :: Parser Def
  parseDef str = do
      PR name rest1 <- parseName str
      PR _ rest2 <- parseChar '=' rest1
      PR exp rest3 <- parseExp rest2
      parseEnd rest3
      return $ PR (Def name exp) rest3


  printDefs :: [Def] -> IO ()
  printDefs [] = return ()
  printDefs (def : defs) = do
    putStrLn (show def)
    printDefs defs





  main :: IO ()
  main = do

      -- let testInput = "if(stuff(x),y,z)"
      -- let result = parseIf testInput
      -- print result
      -- putStrLn ""


      -- putStrLn "Parsed cons:"
      -- let testInput2 = "5"
      -- let r1 = parseConst testInput2
      -- print r1

      -- putStrLn "Parsed 11cons:"
      -- let testInput2 = "1.4a"
      -- let r2 = parseExp "a("
      -- print r2

      putStrLn "parseDef "
      print $ parseDef "fff=g(3.3,ff(23.4,0.0))"


      -- putStrLn "Parsed cons:"
      -- let testInput2 = "1.4a"
      -- let r3 = parseExp testInput2
      -- print r3

      -- runtime

      putStrLn ""
      contents <- readFile "program.txt"
      putStrLn "Original contents:"
      putStrLn contents

      let linesOfFile = lines contents
      let trimmedLines = map (filter (/= ' ')) linesOfFile

      putStrLn "Trimmed and arrayed:"
      print trimmedLines

      putStrLn "Parsed definitions:"
      putStrLn ""
      processLines trimmedLines



  -- processLines :: [String] -> IO ()
  -- processLines [] = return ()
  -- processLines (line:lines) = do

  --     putStrLn $ "Parsing line: " ++ line
  --     case parseDef line of
  --         Just (PR def _) -> do
  --             putStrLn (show def)
  --         Nothing -> putStrLn ("FAILED " ++ line)
  --     processLines lines


  processLines :: [String] -> IO ()
  processLines [] = return ()
  processLines (line:lines) = do
      putStrLn $ "Parsing line: " ++ line
      case parseDef line of
          Just (PR def _) -> printDefs [def]  -- Pass the single Def as a list
          Nothing         -> putStrLn ("FAILED to parse: " ++ line)
      processLines lines
