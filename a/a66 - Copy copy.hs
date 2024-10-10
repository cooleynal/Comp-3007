import A6

-- Parse an expression
parseExp :: Parser Exp

-- parseExp = parseConst +++ parseVar +++ parseIf +++ parseApp1 +++ parseApp2


parseExp = parseIf +++ parseApp2 +++ parseApp1 +++ parseVar +++ parseConst
-- parseExp = parseApp2 +++ parseApp1 +++ parseConst +++ parseVar



-- Parse a constant (number)
parseConst :: Parser Exp
parseConst str = do
  PR digits rest1 <- parseDigits str
  let constValue = read digits :: Double
  return $ PR (Const constValue) rest1

-- Parse a variable
parseVar :: Parser Exp
parseVar str = do
  PR name rest <- parseName str
  return $ PR (Var name) rest

-- Parse an operator name (functions like plus, times)
parseOpName :: Parser String
parseOpName str = do
  PR name rest <- parseName str
  failUnless $ name `elem` ["plus", "times"]
  return $ PR name rest

-- Parse an "if" statement
parseIf :: Parser Exp
parseIf str = do
  PR _ rest1 <- parseOpName str
  PR cond rest2 <- parseExp rest1
  PR _ rest3 <- parseChar ',' rest2
  PR thenExp rest4 <- parseExp rest3
  PR _ rest5 <- parseChar ',' rest4
  PR elseExp rest6 <- parseExp rest5
  return $ PR (If cond thenExp elseExp) rest6

-- Parse a function application with two arguments
parseApp2 :: Parser Exp
parseApp2 str = do
  PR funcName rest1 <- parseOpName str
  PR _ rest2 <- parseChar '(' rest1
  PR arg1 rest3 <- parseExp rest2
  PR _ rest4 <- parseChar ',' rest3
  PR arg2 rest5 <- parseExp rest4
  PR _ rest6 <- parseChar ')' rest5
  return $ PR (App2 funcName arg1 arg2) rest6


-- Parse a function application with one argument
parseApp1 :: Parser Exp
parseApp1 str = do
  PR funcName rest1 <- parseName str
  PR _ rest2 <- parseChar '(' rest1
  PR arg rest3 <- parseExp rest2
  PR _ finalRest <- parseChar ')' rest3
  return $ PR (App1 funcName arg) finalRest


-- Parse a definition (name = expression)
parseDef :: Parser Def
parseDef str = do
  PR name rest1 <- parseName str
  PR _ rest2 <- parseChar '=' rest1
  PR exp rest3 <- parseExp rest2
  parseEnd rest3
  return $ PR (Def name exp) rest3


-- Main function to read and parse the file
main :: IO ()
main = do
  contents <- readFile "program.txt"
  putStrLn "Original contents:"
  putStrLn contents

  let linesOfFile = lines contents
  let trimmedLines = map (filter (/= ' ')) linesOfFile

  putStrLn "Trimmed and arrayed:"
  print trimmedLines

  putStrLn "Parsed definitions:"
  processLines trimmedLines

processLines :: [String] -> IO ()
processLines [] = return ()
processLines (line:lines) = do
  let result = parseDef line
  case result of
    Just (PR def _) -> putStrLn (show def)
    Nothing         -> putStrLn ("Parsing failed for line: " ++ line)
  processLines lines
