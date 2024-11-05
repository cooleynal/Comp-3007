-- Quiz 2 (10 pts added to your score, total capped at 100%)

-- Replace the "undefined" parts with your own code. Each question has a test
-- after it that should evaluate to True.

-- Do not use list comprehension.

-- The questions all have equal weight.

-- The questions all have an indicated difficulty level:

-- E = Easier. Short, direct application of basic assignment techniques
-- M = Moderate. Similar to average assignment question, but shorter code.
-- H = Harder. Ranging from a bit above Moderate to challenging.
--
-- You might find the test long, so make sure you do the easier questions first!

-- Coding restrictions. The questions all come with resrictions on how you
-- write your code. We will review all the quizzes afterwards and non-complying
-- code will be rejected.

-- The restrictions are all as in the relevant assignment. Specifically, they
-- are: 1) using foldr instead of list recursion/pattern-matching, 2) using
-- tail recursion, and 3) using "do" for handling the Maybe representation of
-- failing computations.

-- Question 1
-- Difficulty: E
-- Use foldr to add all the positive numbers in a list of integers.
-- addPos :: [Int] -> Int
-- addPos l = foldr add 0 l
--   where add ele acc = ele + acc

import Debug.Trace (trace)


-- foldr t => (a -> b -> b) -> b -> t a -> b
--            (ele -> acc -> acc) -> acc_start -> list l -> acc_end

-- Difficulty: E
-- Use foldr to add all the positive numbers in a list of integers.
-- DONE
addPos :: [Int] -> Int
addPos l = foldr add 0 l
  where
    -- add :: Int -> Int -> Int
    add ele acc -- (a -> b -> b) (ele -> acc -> acc)
      | ele > 0   = ele + acc
      | otherwise = acc




-- addPos :: [Int] -> Int
-- addPos l = foldr add 0 l

-- add :: Int -> Int -> Int
-- add ele acc
--   | ele > 0   = ele + acc
--   | otherwise = acc



addPosTest :: Bool
addPosTest = addPos [1, 2, -3, 4] == 7

-- Question 2
-- Difficulty: M
-- Use insert and foldr to sort a list of integers.
-- sort :: [Int] -> [Int]
-- sort =  undefined

-- SORT DONE
-- sort :: (Ord a) => [a] -> [a]
-- sort [] = []
-- sort (x:xs) =
--     let smallerSorted = sort [a | a <- xs, a <= x]
--         biggerSorted = sort [a | a <- xs, a > x]
--     in  smallerSorted ++ [x] ++ biggerSorted

-- sort :: [Int] -> [Int]
-- sort [] = []
-- sort l = insert (head l) (sort (tail l))


-- sort [4, 2, 3, 1]
sort :: [Int] -> [Int]
sort l = foldr insert [] l

-- each element a, b is the accumulator
-- (a -> b -> b)
insert :: Int -> [Int] -> [Int]
insert k [] = [k]
insert k (x : l) | k <= x = k : x : l
insert k (x : l) = x : insert k l

sortTest = sort [4, 2, 3, 1] == [1, 2, 3, 4]



-- Question 3
-- Difficulty: H
-- Test if a list is reverse-sorted, i.e. sorted in non-increasing order.
-- isRevSorted :: [Int] -> Bool
-- isRevSorted l =
--   undefined

-- DONE
-- isRevSorted :: [Int] -> Bool
-- isRevSorted [] = True
-- isRevSorted (x : xl)
--   | length xl > 0 = x > head xl && isRevSorted xl -- is safe for empty case
--   | otherwise = True -- needed but poor use

isRevSorted :: [Int] -> Bool
isRevSorted [] = True
isRevSorted (_ : []) = True
isRevSorted (x : xl) = x > head xl && isRevSorted xl



-- thinks needs touple to use foldr on sorting a list




-- isRevSorted :: [Int] -> Bool
-- isRevSorted [] = True
-- isRevSorted [_] = True
-- isRevSorted (x : y : xs)
--   | x >= y          = isRevSorted xs
--   | otherwise       = False

-- init, all but last
-- tail, all but first
-- isRevSorted :: [Int] -> Bool
-- isRevSorted [] = True
-- isRevSorted [x] = True
-- isRevSorted xs = foldr check True (zip (init xs) (tail xs))
--   where
--     check (prev, curr) acc =
--       trace ("curr: " ++ show curr ++ ", prev: " ++ show prev ++ ", acc: " ++ show acc)
--       (curr <= prev && acc)

-- isRevSorted :: [Int] -> Bool
-- isRevSorted [] = True
-- isRevSorted [_] = True
-- isRevSorted xs = foldr check True (zip (init xs) (tail xs))
--   where
--     check (prev, curr) acc = curr <= prev && acc




isRevSortedTest0 = isRevSorted [4, 3, 2, 1]

isRevSortedTest1 = not $ isRevSorted [4, 2, 3, 1]

-- Question 4
-- Difficulty: E
-- Use tail recursion to add all the positive numbers in a list of integers.

-- DONE
sumPos :: [Int] -> Int
sumPos l =
  sumPos0 l 0

sumPos0 :: [Int] -> Int -> Int
sumPos0 [] acc = acc
sumPos0 (x : xs) acc
  | x > 0       = sumPos0 xs (x + acc) -- needs to be wraped, otherwise bad
  | otherwise   = sumPos0 xs acc

sumPosTest = sumPos [1, 2, -3, 4] == 7






-- Question 5
-- Difficult: M
-- Write a tail recursive version of the split function from Assignment 4 (code supplied below).
split :: (a -> Bool) -> [a] -> ([a], [a])
split p l = split0 p l ([], [])
                    -- predicate    l          rest (false, True)
-- split0 :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
-- split0 = undefined

                    -- predicate    l          rest (true, false)
split0 :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
split0 p [] (a, b) = (a, b) -- could sort here, not needed according to example
split0 p (x : xl) (a, b) | p x  = split0 p xl (x : a, b)
split0 p (x : xl) (a, b) = split0 p xl (a, x : b)



-- This is a renaming of the split function from Assignment 4.
splitR :: (a -> Bool) -> [a] -> ([a], [a])
splitR p [] = ([], [])
splitR p (x : l) =
  if p x then (x : l1, l2) else (l1, x : l2)
  where
    (l1, l2) = splitR p l

setEq :: (Eq a) => [a] -> [a] -> Bool
setEq l0 l1 = all (`elem` l0) l1 && all (`elem` l1) l0

pairSetEq :: (Eq a, Eq b) => ([a], [b]) -> ([a], [b]) -> Bool
pairSetEq (l00, l01) (l10, l11) = setEq l00 l10 && setEq l01 l11

splitTest = split (<= 3) [1, 4, 3, 5, 2] `pairSetEq` ([1, 3, 2], [4, 5])




-- DONE

-- Question 6
-- Difficulty: E
-- Write a version of the built-in head function that indicates an error using
-- Maybe instead of raisihg an exception.
-- hd :: [a] -> Maybe a
-- hd [] = Nothing
-- hd l = do Just (head l)


hd :: [a] -> Maybe a
hd []         = Nothing
hd (x : _)    = Just x



hdTest = hd "123" == Just '1' && hd "" == Nothing






-- Question 7
-- Difficulty: E
-- Look up a key in a map,
-- then look up the key's value in a second map,

-- using Maybe to deal with errors.

-- Coding restriction: your code should just be a sequence of lines in the "do".

-- "Just" and "Nothing" should not appear in your code.
lookup2 :: (Eq a, Eq b) => a -> [(a, b)] -> [(b, c)] -> Maybe c
lookup2 x m1 m2 = do
  v1 <- lookup x m1
  v2 <- lookup v1 m2
  return v2

lookup2Test = lookup2 1 [(0, 1), (1, 2), (2, 3)] [(0, 1), (1, 2), (2, 3)] == Just 3

-- Question 8
-- Difficulty: M
-- The solution to the parser assignment is included at the bottom of this file.
-- The language has been extended with a constructor "HexConst". Instead of just
-- floating point values, hex values are included. These values are written with
-- an "x" preceding them, e.g. "x3ED4F". More precisely, they are strings starting
-- with "x" followed by characters satisfying isHexChar (see below).

-- parseHexConst :: Parser Exp
-- parseHexConst str = do


-- DONE

parseHexConst :: Parser Exp
parseHexConst str = do
  PR _ rest1 <- parseChar 'x' str
  PR hex rest2 <- parseHex rest1
  return $ PR (HexConst hex) rest2


parseHex :: Parser String
parseHex = collect isHexChar


isHexChar :: Char -> Bool
isHexChar c = c `elem` "0123456789ABCDEF"

parseHexConstTest = parseHexConst "x33AF0(22.2)" == Just (PR (HexConst "33AF0") "(22.2)")

-- parseHexConst "x33AF0(22.2)"




-- Question 9 -- EXTRA CREDIT (10 pts added to your score, total capped at 100%)
-- Difficulty: H
-- The language has been extended with "List" (see the Exp type). Lists are
-- expressions that comprise a sequence of expressions separated by commas and
-- surrounded by "[" and "].
-- Hint: consider writing several independent parsers for different cases of
-- list length, and combine them using +++.
-- parseList str = do
--   undefined


-- parseList :: Parser Exp
-- parseList str = do
--   trace ("Input string: " ++ str) $ return ()

--   PR _ r1     <- parseChar '[' str
--   trace ("After parsing '[': " ++ show r1) $ return ()

--   PR ex1 r2   <- parseExp r1
--   trace ("Parsed ex1: " ++ show ex1 ++ ", remaining: " ++ r2) $ return ()

--   PR _ r3     <- parseChar ',' r2
--   trace ("After parsing first ',': " ++ r3) $ return ()

--   PR ex2 r4   <- parseExp r3
--   trace ("Parsed ex2: " ++ show ex2 ++ ", remaining: " ++ r4) $ return ()

--   PR _ r5     <- parseChar ',' r4
--   trace ("After parsing second ',': " ++ r5) $ return ()

--   PR ex3 r6   <- parseExp r5
--   trace ("Parsed ex3: " ++ show ex3 ++ ", remaining: " ++ r6) $ return ()

--   PR _ r7     <- parseChar ']' r6
--   trace ("After parsing ']': " ++ r7) $ return ()

--   -- return $ PR (List ([] : ex1 ++ [ex2] ++ [ex3]) ) r7
--   -- return $ PR (List [ex1, ex2, ex3]) r7
--   return $ PR ( List (ex1 : ex2 : ex3 : []) ) r7



-- parseList :: Parser Exp
-- parseList str = do
--   PR _ r1     <- parseChar '[' str

--   PR ex1 r2   <- parseExp r1

--   PR _ r3     <- parseChar ',' r2
--   PR ex2 r4   <- parseExp r3

--   PR _ r5     <- parseChar ',' r4
--   PR ex3 r6   <- parseExp r5

--   PR _ r7     <- parseChar ']' r6
--   -- return $ PR (List ([] : ex1 ++ [ex2] ++ [ex3]) ) r7
--   -- return $ PR (List [ex1, ex2, ex3]) r7
--   return $ PR ( List (ex1 : ex2 : ex3 : []) ) r7

-- parseList :: Parser Exp
-- parseList str = do
--   PR _ r1 <- parseChar '[' str
--   PR elements r2 <- parseListArgs r1
--   return (PR (List elements) r2)



-- parseListArg str = do
--   PR arg rest0 <- parseExp str
--   PR _ rest1 <- parseChar ',' rest0
--   PR args rest2 <- parseListArgs rest1
--   return $ PR (arg : args) rest2

parseList str = do
  PR _ rest0 <- parseChar '[' str
  PR l rest1 <- parseListArgs rest0
  return (PR (List l) rest1)


parseListArg str = do
  PR arg rest0 <- parseExp str
  PR _ rest1 <- parseChar ',' rest0
  PR args rest2 <- parseListArgs rest1
  return $ PR (arg : args) rest2


parseLastListArg str = do
  PR arg rest0 <- parseExp str
  PR _ rest1 <- parseChar ']' rest0
  return $ PR [arg] rest1

parseEndListArgs str = do
  PR _ rest0 <- parseChar ']' str
  return $ PR [] rest0



parseListArgs :: Parser [Exp]
parseListArgs = parseEndListArgs +++ parseLastListArg +++ parseListArg


parseListTest = parseList "[y,22.0,z(w)]" == Just (PR (List [Var "y", Const 22.0, App1 "z" (Var "w")]) "")

-------------------------------
-- Assignment 6 solution below
-------------------------------

type Name = String

data Exp
  = Const Double
  | HexConst String
  | List [Exp]
  | Var String
  | If Exp Exp Exp
  | App1 Name Exp
  | App2 Name Exp Exp
  deriving (Show, Eq)

data Def
  = Def Name Exp
  deriving (Show, Eq)

type Parser a = String -> Maybe (PR a)

data PR a = PR a String
  deriving (Show, Eq)

reservedWords = words "if x"

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

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 =
  \str -> case p1 str of
    Nothing -> p2 str
    Just val -> Just val

parseEnd :: Parser ()
parseEnd s = do
  failIf $ s /= ""
  return $ PR () ""

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

isLetter :: Char -> Bool
isLetter c = c `elem` "abcdefghijklmnopqrstuvwxyz"

parseChar :: Char -> Parser Char
parseChar c str = do
  (c', rest) <- maybeCons str
  failUnless $ c == c'
  return $ PR c rest

maybeCons :: [a] -> Maybe (a, [a])
maybeCons (x : l) = Just (x, l)
maybeCons _ = Nothing

collect :: (Char -> Bool) -> Parser String
collect p str = do
  if headIs p str
    then do
      (c, rest) <- maybeCons str
      PR collected rest' <- collect p rest
      return $ PR (c : collected) rest'
    else do
      return $ PR [] str

parseName :: Parser Name
parseName = collect isLetter

parseDigits :: Parser String
parseDigits = collect isDigit

parseExp :: Parser Exp
parseExp =
  parseConst
    +++ parseHexConst
    +++ parseList
    +++ parseVar
    +++ parseIf
    +++ parseApp1
    +++ parseApp2

parseConst :: Parser Exp
parseConst str = do
  PR leftOfPoint rest0 <- parseDigits str
  PR _ rest1 <- parseChar '.' rest0
  PR rightOfPoint rest2 <- parseDigits rest1
  let numberStr = leftOfPoint ++ "." ++ rightOfPoint
  let number = read numberStr :: Double
  return $ PR (Const number) rest2

parseVar :: Parser Exp
parseVar str = do
  PR name rest <- parseName str
  failIf (name `elem` reservedWords)
  failIf (headIs (== '(') rest)
  return $ PR (Var name) rest

parseOpName :: Parser String
parseOpName str = do
  PR name rest <- parseName str
  failIf (name `elem` reservedWords)
  failIf (headIs (/= '(') rest)
  return $ PR name rest

parseIf :: Parser Exp
parseIf str = do
  PR name rest0 <- parseName str
  failIf (name /= "if")
  PR _ rest1 <- parseChar '(' rest0
  PR e0 rest2 <- parseExp rest1
  PR _ rest3 <- parseChar ',' rest2
  PR e1 rest4 <- parseExp rest3
  PR _ rest5 <- parseChar ',' rest4
  PR e2 rest6 <- parseExp rest5
  PR _ rest7 <- parseChar ')' rest6
  return $ PR (If e0 e1 e2) rest7

parseApp2 :: Parser Exp
parseApp2 str = do
  PR name rest0 <- parseName str
  PR _ rest1 <- parseChar '(' rest0
  PR e0 rest2 <- parseExp rest1
  PR _ rest3 <- parseChar ',' rest2
  PR e1 rest4 <- parseExp rest3
  PR _ rest5 <- parseChar ')' rest4
  return $ PR (App2 name e0 e1) rest5

parseApp1 :: Parser Exp
parseApp1 str = do
  PR name rest0 <- parseName str
  PR _ rest1 <- parseChar '(' rest0
  PR e0 rest2 <- parseExp rest1
  PR _ rest3 <- parseChar ')' rest2
  return $ PR (App1 name e0) rest3

parseDef :: Parser Def
parseDef str = do
  PR name rest0 <- parseName str
  failIf (name `elem` reservedWords)
  PR _ rest1 <- parseChar '=' rest0
  PR e rest2 <- parseExp rest1
  return $ PR (Def name e) rest2
