import A6

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
    if opName /= "if"
        then Nothing
        else do
            PR _ rest2 <- parseChar '(' rest1
            PR cond rest3 <- parseExp rest2
            PR _ rest4 <- parseChar ',' rest3
            PR thenExp rest5 <- parseExp rest4
            PR _ rest6 <- parseChar ',' rest5
            PR elseExp rest7 <- parseExp rest6
            PR _ finalRest <- parseChar ')' rest7
            return $ PR (If cond thenExp elseExp) finalRest

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


parseApp1 :: Parser Exp
parseApp1 str = do
    PR funcName rest1 <- parseName str
    PR _ rest2 <- parseChar '(' rest1
    PR arg rest3 <- parseExp rest2
    -- PR arg rest3 <- parseVar rest2
    PR _ finalRest <- parseChar ')' rest3
    return $ PR (App1 funcName arg) finalRest

parseDef :: Parser Def
parseDef str = do
    PR name rest1 <- parseName str
    PR _ rest2 <- parseChar '=' rest1
    PR exp rest3 <- parseExp rest2
    parseEnd rest3
    if null name
      then Nothing
      else return $ PR (Def name exp) rest3

main :: IO ()
main = do
    contents <- readFile "program.txt"
    putStrLn "Original contents:"
    putStrLn contents

    -- putStrLn parseOpName "w(x)"

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
    putStrLn $ "Parsing line: " ++ line
    case parseDef line of
        Just (PR def _) -> do
            putStrLn (show def)
        Nothing -> putStrLn ("FAILED " ++ line)
    processLines lines
