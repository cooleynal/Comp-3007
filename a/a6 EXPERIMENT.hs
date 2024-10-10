import A6

-- Parse an expression
parseExp :: Parser Exp

-- parseExp = parseConst +++ parseVar +++ parseIf +++ parseApp1 +++ parseApp2

parseExp = parseIf +++ parseApp1 +++ parseApp2 +++ parseConst +++ parseVar




parseConst :: Parser Exp
parseConst str = do
  PR intPart restAfterDigits <- parseDigits str
  PR _ restAfterDecimal <- parseChar '.' restAfterDigits
  PR fracPart restAfterFrac <- parseDigits restAfterDecimal
  let numberString = intPart ++ "." ++ fracPart
  let number = read numberString :: Double

  return $ PR (Const number) restAfterFrac



-- variable
-- parseVar :: Parser Exp
-- parseVar str = do
--   PR name rest <- parseName str
--   return $ PR (Var name) rest

parseVar :: Parser Exp
parseVar str = do
  PR name rest <- parseName str
  if name == ""
    then Nothing
  else return  (PR (Var name) rest)

-- not checking if open (
-- make sure not closing bracket
parseOpName :: Parser String
parseOpName str = do
  PR name rest <- parseName str
  if name == ""
    then Nothing
  else Just (PR name rest)


-- do
parseIf :: Parser Exp
parseIf str = do
    PR op rest1 <- parseOpName str
    PR cond rest2 <- parseExp rest1
    PR _ rest3 <- parseChar ',' rest2
    PR thenExp rest4 <- parseExp rest3
    PR _ rest5 <- parseChar ',' rest4
    PR elseExp rest6 <- parseExp rest5
    -- probably doesnt correctly return Nothing
    if op /= "if" || null (show cond) || null (show thenExp) || null (show elseExp)
      then return Nothing Nothing
      else return $ PR (If cond thenExp elseExp) rest6




-- function application with two arguments
parseApp2 :: Parser Exp
parseApp2 str = do
  PR funcName rest1 <- parseOpName str
  PR _ rest2 <- parseChar '(' rest1
  PR arg1 rest3 <- parseExp rest2
  PR _ rest4 <- parseChar ',' rest3
  PR arg2 rest5 <- parseExp rest4
  PR _ rest6 <- parseChar ')' rest5
  if null funcName
    then Nothing
  else return $ PR (App2 funcName arg1 arg2) rest6


-- function application with one argument
parseApp1 :: Parser Exp
parseApp1 str = do
  PR funcName rest1 <- parseName str
  PR _ rest2 <- parseChar '(' rest1
  PR arg rest3 <- parseExp rest2
  PR _ finalRest <- parseChar ')' rest3
  if null funcName
    then Nothing
  else return $ PR (App1 funcName arg) finalRest


-- definition (name = expression)
parseDef :: Parser Def
parseDef str = do
    PR name rest1 <- parseName str
    PR _ rest2 <- parseChar '=' rest1
    PR exp rest3 <- parseExp rest2
    parseEnd rest3
    if null name
      then Nothing
    else return $ PR (Def name exp) rest3


-- Main function to read and parse the file
main :: IO ()
main = do

  let input = "asdf"
  let result = parseOpName input
  print result

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

processLines :: [String] -> IO ()
processLines [] = return ()
processLines (line:lines) = do

  case parseDef line of
    Just (PR def _) -> do
      putStrLn (show def)
    Nothing -> putStrLn ("Parsing failed for line: " ++ line)

  processLines lines



-- Def "f" (App2 "plus" (App2 "times" (Const 3.3) (Const 23.4)) (Const 0.0))
-- Def "g" (Var "x")
-- Def "h" (App2 "f" (App1 "f" (App2 "g" (Var "x") (Var "y"))) (App1 "h" (Var "z")))
