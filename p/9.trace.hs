import Debug.Trace (trace)
import Prelude hiding (head)





type Name = String

-- Expressions
data Exp
  = Const Double
  | HexConst String
  | List [Exp]
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


parseExp :: Parser Exp
parseExp = parseConst +++ parseVar +++ parseIf +++ parseApp1 +++ parseApp2

parseConst :: Parser Exp
parseConst str = do
    PR intPart restAfterDigits <- parseDigits str
    trace ("int: " ++ show intPart) $ return ()
    PR _ restAfterDecimal <- parseChar '.' restAfterDigits
    PR fracPart restAfterFrac <- parseDigits restAfterDecimal
    trace ("PR fracPart restAfterFrac: " ++ show fracPart ++ " " ++ restAfterFrac) $ return ()
    let numberString = intPart ++ "." ++ fracPart
    let number = read numberString :: Double
    trace ("Const number: " ++ show number) $ return ()
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
    trace ("if condition: " ++ show cond) $ return ()
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


parseList :: Parser Exp
parseList str = do
  trace ("Input string: " ++ str) $ return ()

  PR _ r1     <- parseChar '[' str
  trace ("After parsing '[': " ++ show r1) $ return ()

  PR ex1 r2   <- parseExp r1
  trace ("Parsed ex1: " ++ show ex1 ++ ", remaining: " ++ r2) $ return ()

  PR _ r3     <- parseChar ',' r2
  trace ("After parsing first ',': " ++ r3) $ return ()

  PR ex2 r4   <- parseExp r3
  trace ("Parsed ex2: " ++ show ex2 ++ ", remaining: " ++ r4) $ return ()

  PR _ r5     <- parseChar ',' r4
  trace ("After parsing second ',': " ++ r5) $ return ()

  PR ex3 r6   <- parseExp r5
  trace ("Parsed ex3: " ++ show ex3 ++ ", remaining: " ++ r6) $ return ()

  PR _ r7     <- parseChar ']' r6
  trace ("After parsing ']': " ++ r7) $ return ()

  -- return $ PR (List ([] : ex1 ++ [ex2] ++ [ex3]) ) r7
  -- return $ PR (List [ex1, ex2, ex3]) r7
  return $ PR ( List (ex1 : ex2 : ex3 : []) ) r7


parseListTest = parseList "[y,22.0,z(w)]" == Just (PR (List [Var "y", Const 22.0, App1 "z" (Var "w")]) "")





factorial :: Int -> Int
factorial 0 = 1
factorial n = trace ("factorial " ++ show n) (n * factorial (n - 1))

main :: IO ()
main = do
    -- print (factorial 5)

    -- let testInput = "if(stuff(x),y,z)"
    let result = parseConst "a"
    print result
    putStrLn ""
