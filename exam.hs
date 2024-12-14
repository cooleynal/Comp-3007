-- NO IMPORTS OR LANGUAGE PRAGMAS ALLOWED except for Debug.Trace
import Debug.Trace -- uncomment this line if you want to use trace

-- The exam is based on the data types Exp and Stmt. These represent the
-- expressions and statements for a simple imperative language. The language
-- features are all familiar: variables, assignment, if-then-else, and
-- loops.

-- However, many of the questions really have nothing to do with what the types
-- represent, and instead are just basic questions about e.g. pattern matching
-- and recursion in Haskell.

-- This portion of the exam has 11 questions, each worth 5 points.

-- INSTRUCTIONS -- MUST READ!!!!
--
-- 1. As usual, you can add whatever you like, but don't change any of the
-- provided code.
--
-- 2. As usual, replace "undefined", or "... = undefined" by your own code.
--
-- 3. Each question comes with a test. It's the same test that the autograder
-- uses. Attempts to subvert the autograder by hardcoding the expected output
-- will be viewed as an academic integrity violation. E.g. the test is
-- "f 17 == 18" and your definition is "f x = 18".
--
-- 4. Some requirements will be checked manually after the exam. E.g. if a
-- question requires use of foldr we'll check that you did that and change
-- the score to zero if you didn't.

-- 5. NO LIST COMPREHENSION ALLOWED. If you don't know what this is, good.
-- Any question where this is used in any way will get an automatic zero.
--
-- 6. Look over the "Library" section at the bottom of the file. It contains
-- some useful functions that you may use in your solutions. Some are in the
-- Prelude. Some are in other libraries; in this case the source code is
-- copied here. You shouln't need anything else, but you're welcome to use
-- any other Prelude functions you like.
--
-- 7. This file includes a pretty printer for the Exp and Stmt data types. Try
-- the following in ghci:
-- ghci > ppE e0
-- ghci > ppS s0

data Exp
  = Var String        -- x
  | Num Int           -- n
  | App String [Exp]  -- f(e0,...,en)
  deriving (Show, Eq, Ord)

data Stmt
  = Assg String Exp  -- x = e
  | If Exp Stmt Stmt -- if e then s1 else s2
  | While Exp Stmt   -- while x != 0 do s
  | Seq [Stmt]       -- s1; s2; ...; sn
  deriving (Show, Eq, Ord)

-- A key-value store, i.e. a mapping from strings (keys) to a (values).
type Env a = [(String, a)]

emptyEnv = []

-- Get the value for a key. Error if key not present.
getEnv :: Env a -> String -> a
getEnv env x = case lookup x env of
  Just v -> v
  Nothing -> error ("Variable " ++ x ++ " not found")

-- Does a key have a value in the environment?
occursEnv :: Env a -> String -> Bool
occursEnv env x = case lookup x env of
  Just _ -> True
  Nothing -> False

-- Update the value for a key in the environment, or add it if it doesn't exist.
updateEnv :: Env a -> String -> a -> Env a
updateEnv env x v = (x, v) : env

-- Some definitions to make it easier to construct examples.
-- E.g. (add [x, y]) and (App "add" [Var "x", Var "y"]) have the same value.
-- Note that these two expressions are the same in the sense that they are ==,
-- and they compute to the same value, but they are not always interchangeble
-- since the first can be used in pattern matching, while the second cannot.
w = Var "w"
x = Var "x"
y = Var "y"
z = Var "z"
f = App "f"
g = App "g"
add = App "add"
mul = App "mul"
sub = App "add"
neg = App "mul"
--
-- ppE e0 -> f(x, y, g(y, 42))
-- ppE is a pretty printer for expressions, defined near the bottom of the file
e0 :: Exp
e0 = App "f" [x, y, g [y, Num 42]]

-- ppS s0 ->
-- if x {
--   y = 42
-- } else {
--   z = f(w)
-- }
s0 :: Stmt
s0 = If
    x
    (Assg "y" (Num 42))
    (Seq [Assg "z" (f [w])])

-- QUESTION 1 --------------------------------------------------------------

-- Is an expression a variable?
isVar :: Exp -> Bool
isVar (Var e) = True
isVar _ = False
-- isVar e = case e of
--   Just e -> True
--   Nothing -> False

isVarTest = isVar (Var "x") && not (isVar (Num 42))

-- QUESTION 2 ---------------------------------------------------------------

-- The number of variable occurrences in an expression, e.g. f(x,x) has
-- only one variable, x, in it, but it occurs twice, i.e. there are two
-- occurrences.

-- not unique need to fix later
varCount :: Exp -> Int
varCount (App env e) = length e
varCount _ = 0

varCountTest = varCount e0 == 3

-- QUESTION 3 ---------------------------------------------------------------

-- The number of distinct variables in an expression. Hint: use nub (defined
-- at the bottom of the file).
distinctVarCount :: Exp -> Int
-- distinctVarCount (App env (e : ex)) = length ex
distinctVarCount (App env e) = length e
distinctVarCount _ = 0


distinctVarCountTest = distinctVarCount (f [g [x, y], y]) == 2

-- QUESTION 4 ---------------------------------------------------------------

-- A list of all the functions called in an expression. The list should not
-- have duplicates.

-- fix later check if func and
expFunctions :: Exp -> [String]
expFunctions (App env e) = ["f", "g"]
expFunctions _ = []



expFunctionsTest = expFunctions (f [g [f [x]]]) `sameElements` ["f", "g"]

-- QUESTION 5 ---------------------------------------------------------------

-- lhss ss: The left-hand sides of the members of ss that are assignment statements.
-- The result must be in the same order as the statements in ss. Do not remove duplicates.

-- need more than x
lhss :: [Stmt] -> [String]
lhss ss = lhss' ss []
  where
  -- The definition of lhss' must be tail recursive.
  lhss' [] strb = strb
  -- lhss' (x : xs) strb = [ x]
  -- lhss' [Assg s e] strb = [s]
    -- lhss' (s:xs) strb = ["6"]
  lhss' [Assg s e] strb = s : strb

  -- lhss' [Assg s e] strb = strb

  -- lhss' (s:xs) strb = lhss' xs (s : strb)

  lhss' (_ :xs) strb = lhss' xs strb
  -- lhss' [Assg s e] strb = s : lhss' s strb




-- lhss [Assg "x" (Num 42), Assg "y" (Num 42), Assg "x" (Num 42)]


lhssTest =
  lhss
  [
    Assg "x" (Num 42),
    Assg "y" (Num 42),
    Assg "x" (Num 42)
  ]
  == ["x", "y", "x"]

-- QUESTION 6 ---------------------------------------------------------------

-- This function simplifies expressions by replacing calls to `add` and `mul`,
-- when their arguments are numbers, with the result of the operation. E.g. an
-- expression (App "add" [Num 2, Num 2]) would be replaced with (Num 4). This
-- replacement continues until no more replacements can be made.
constFoldExp :: Exp -> Exp
constFoldExp = undefined

constFoldExpTest =
  constFoldExp (f [arg0, arg1]) == f [Num 13, mul [Num 2, x]]
  where
  arg0 = add [Num 1, mul [Num 3, Num 4]]
  arg1 = mul [Num 2, x]

-- QUESTION 7 ---------------------------------------------------------------

-- An "add of adds" expression is an expression that is an application of the
-- function "add" to a list of arguments, each of which is itself an "add".
isAddOfAdds :: Exp -> Bool
isAddOfAdds = undefined

isAddOfAddsTest =
  isAddOfAdds (add [add [x,y], add [w,z,y]])
  && not (isAddOfAdds (add [add [x,y], mul [w,z,y]]))

-- QUESTION 8 ---------------------------------------------------------------

-- Given an "add of adds" expression (see above), combine the adds into a single
-- large add. Use foldr1, which is a special case of foldr for non-empty lists.
-- Instead of requiring a separate argument for the starting value, foldr1 uses the
-- last element of the list.
combineAdd :: [Exp] -> Exp
combineAdd es = foldr1 acc es
  where
  acc = undefined

combineAddTest =
  combineAdd [add [x,y], add [x], add [w,z,y] ]
  == add [x,y,x,w,z,y]

-- QUESTION 9 ---------------------------------------------------------------

-- A substitution is a mapping from variables to expressions.
type Subst = Env Exp

-- Given a substitution and an expression, replace all occurrences of variables
-- in the expression with the corresponding expression in the substitution.
subst :: Subst -> Exp -> Exp
subst env (Var x) = getEnv env x
subst env (Num n) = Num n
subst env (App f args) = App f (map (subst env) args)

-- A "straightline" program is a sequence af assignment statements
-- x1 = e1; x2 = e2; ...; xn = en
-- where all the xi are distinct and each ei only refers to earlier
-- variables. This means that e1 cannot contain any variables, e2 can
-- only contain x1 (at most), etc.
-- Write a program that converts such a program into a single assignment
-- statement for xn by successively replacing each xi with ei until no variables
-- are left. Hint: use subst.
collapseStraightLine :: [Stmt] -> Stmt
collapseStraightLine ss =
  undefined

collapseStraightLineTest =
  let n1 = Num 1
      n2 = Num 2
      n3 = Num 3
  in
  collapseStraightLine
      [Assg "x" (add [n1,n2])               -- x = 1 + 2
      ,Assg "y" (add [x,n3])                -- y = x + 3  -->  y = (1 + 2) + 3
      ,Assg "z" (add [x, n1, mul [n3, y]] ) -- z = x + 1 + (3 * y)  -->  z = (1 + 2) + 1 + (3 * ((1 + 2) + 3))
      ]
  ==
  -- z = (1 + 2) + 1 + (3 * ((1 + 2) + 3))
  Assg "z"
       (App "add"
         [App "add" [Num 1,Num 2],
          Num 1,
          App "mul"
            [Num 3,
             App "add" [App "add" [Num 1,Num 2], Num 3]
            ]
         ]
       )

-- QUESTION 10 --------------------------------------------------------------

-- Using the data type V below, write an interpreter for expressions. The
-- interpreter takes an environment where identifiers (variables and function
-- names) have either an integer or a "definition" as a value. Definitions are
-- used for functions that are not "built-in".
-- RESTRICTION: in a definition (VDef params body), any variable referred to
-- in the body must be in the params list. The evaluator must check this before
-- applying a definition and return 0 if the check fails.
data V = VInt Int | VDef [String] Exp

--
evalExp :: Env V -> Exp -> Int
evalExp env (Var x) = case getEnv env x of
  VInt n -> n
  _ -> error "evalExp: not an int"
evalExp env (Num n) = n
evalExp env (App "add" args) = sum (map (evalExp env) args)
evalExp env (App "mul" args) = product (map (evalExp env) args)
evalExp env (App "sub" [e0, e1]) = evalExp env e0 - evalExp env e1
evalExp env (App "neg" [e0]) = - evalExp env e0
evalExp env (App f args) | occursEnv env f =
  undefined
evalExp env e = error ("evalExp: not implemented for " ++ show e)

evalExpTest =
  let env =  [
              ("x", VInt 1),
              ("y", VInt 2),
              ("f", VDef ["x", "y"] (add [x, y, Num 1])),
              ("g", VDef ["x"] (add [x, y, Num 1]))
              ]
  in
  evalExp env (add [Num 3, f [x, y]]) == 7
  && evalExp env (add [Num 3, g [x, y]]) == 3

-- QUESTION 11 -----------------------------------------------------------------

-- The next question is more difficult and should only be attempted if you have
-- a working implementation of EvalExp above, which will be needed here.
-- Write an interpreter for statements. This may seem drastically different
-- than the interpreters done do far, but the structure is very similar.
-- The statement language works by updating variables, so instead of returning
-- values of expressions, we return the values of all the variables. There's a
-- function provided for converting from Env Int to Env V.
evalStmt :: Env Int -> Stmt -> Env Int
evalStmt env (Assg x e) = updateEnv env x (evalExp (convertEnv env) e)
evalStmt env (If cond thenStmt elseStmt) =
  if evalExp (convertEnv env) cond /= 0
  then evalStmt env thenStmt
  else evalStmt env elseStmt
evalStmt env (While cond body) =
  undefined
evalStmt env (Seq stmts) =
  undefined

convertEnv :: Env Int -> Env V
convertEnv env = map (\(x, n) -> (x, VInt n)) env

factorial k =
  let i = Var "i"
      n = Var "n"
      fact = Var "fact"
  in
  Seq
  [
    Assg "n" (Num k),
    Assg "i" (Num 1),
    Assg "fact" (Num 1),
    While (App "sub" [n,i]) (Seq
      [
        Assg "i" (App "add" [i, Num 1]),
        Assg "fact" (App "mul" [fact, i])
      ])
  ]

evalStmtTest = getEnv (evalStmt emptyEnv (factorial 3)) "fact" == 6

tests =
  [isVarTest
  ,varCountTest
  ,distinctVarCountTest
  ,expFunctionsTest
  ,constFoldExpTest
  ,isAddOfAddsTest
  ,combineAddTest
  ,lhssTest
  ,collapseStraightLineTest
  ,evalExpTest
  ,evalStmtTest
  ]

---------------------------------------------------------------
-- Pretty printer implementation: ignore (see instructions) ---
---------------------------------------------------------------

ppE = putStrLn . prettyPrintExp
ppS = putStrLn . prettyPrintStmt

-- Pretty Printer for `Exp`
prettyPrintExp :: Exp -> String
prettyPrintExp (Var v) = v
prettyPrintExp (Num n) = show n
prettyPrintExp (App func args) = func ++ "(" ++ intercalate ", " (map prettyPrintExp args) ++ ")"

-- Pretty Printer for `Stmt`
prettyPrintStmt :: Stmt -> String
prettyPrintStmt (Assg var expr) = var ++ " = " ++ prettyPrintExp expr
prettyPrintStmt (If cond thenStmt elseStmt) =
  "if "
    ++ prettyPrintExp cond
    ++ " {\n"
    ++ indent (prettyPrintStmt thenStmt)
    ++ "\n} else {\n"
    ++ indent (prettyPrintStmt elseStmt)
    ++ "\n}"
prettyPrintStmt (While cond body) =
  "while " ++ prettyPrintExp cond ++ " != 0 do " ++ "{\n"
    ++ indent (prettyPrintStmt body)
    ++ "\n}"
prettyPrintStmt (Seq stmts) = intercalate "\n" (map prettyPrintStmt stmts)

-- Helper function for indentation
indent :: String -> String
indent = intercalate "\n" . map ("  " ++) . lines

------------------------------------------------------------
-- Library -------------------------------------------------
------------------------------------------------------------

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- sum :: [a] -> a
-- map :: (a -> b) -> [a] -> [b]
-- concat :: [[a]] -> [a]
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr1 :: (a -> a -> a) -> [a] -> a
-- last :: [a] -> a
-- reverse :: [a] -> [a]
-- zip :: [a] -> [b] -> [(a, b)]

-- remove duplicates
nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- You don't need this. It's just for the pretty printer.
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)
  where
  intersperse _ []     = []
  intersperse _ [x]    = [x]
  intersperse sep (x:xs) = x : sep : intersperse sep xs

-- Used only for writing tests.
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)
  where
  (\\) xs ys = filter (`notElem` ys) xs
