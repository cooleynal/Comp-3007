{-# LANGUAGE ImportQualifiedPost #-}

module SchemeEval where

-- import Text.Pretty.Simple (pPrint)
-- import Debug.Trace (trace)

import Data.List (find, intercalate, nub)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import SchemeSyntax

-------- DON'T TOUCH ANYTHING ON THIS LINE OR ABOVE -----------------

-------- DON'T CHANGE THE NAME OF THIS FILE -------------------------

-- This file cantains a slightly-modified version of the eval-based interpreter
-- for Scheme from the lectures.  The assignment is to extend the interpreter
-- to handle a few more Scheme language constructs. All you need to do is add
-- clauses to the definitions of eval and evalApply. You can define "helpers"
-- of course. Be sure to look over the file for existing helpful functions.

-- The constructs are explained in the Scheme file that comes with the
-- assignment. Don't rename the file, and keep it in the same directory/folder
-- as your cod in the same directory/folder as your code.

-- The Scheme file has comments that explain what the new constructs do. One of
-- the constructs is "lambda" for making an unnamed function. Its value will be
-- a closure. There is also a "lambdaf", which has the same meaning in Scheme
-- as  "lambda", except that it is interpreted as a VFunc instead of a
-- VClosure. See the definition of V for more on VFunc.

-- Recursive functions are handled somewhat differently than in class. The
-- VClosure constructor now takes an additional argument, of type String. If the
-- string is empty, then it is an ordinary closure required no special
-- treatment for recursion. Otherwise, the string is the name of a function and
-- the closure is the value assigned to the function. Since definitions are
-- allowed to be recursive, running the closure must use the closure's
-- environment, as usual, but with an added entry for the named function.

-- For testing, use "run". This takes a string, parses it and evaluates it in
-- an environment containing the definitions from the Scheme file. You can run
-- the file's tests using this, e.g. run "(test-letrec-0)".

-- The type of values that can result from evaluating a Scheme program.
data V
  = VNumber Int
  | VString String
  | VBool Bool
  | VNil
  | VCons V V
  | VClosure String Env [String] Exp -- fnName, env, parms, body
  | VFunc ([V] -> V)

-- E.g. the value of "(cons 1 2)" shows as "(1 . 2)". This is the
-- usual form in Lisp-related languages.
instance Show V where
  show (VNumber n) = show n
  show (VString s) = show s
  show (VBool b) = show b
  show (VNil) = "[]"
  show (VCons v0 v1) = "(" ++ show v0 ++ " . " ++ show v1 ++ ")"
  show (VClosure _ _ _ _) = "#"

instance Eq V where
  VNumber m == VNumber n = m == n
  VBool m == VBool n = m == n
  VString m == VString n = m == n
  VNil == VNil = True
  VCons x y == VCons x' y' = x == x' && y == y'
  _ == _ = False

-- Environments map strings/names/identifiers/atoms to values.
newtype Env = Env {envMap :: M.Map String V} deriving (Show, Eq)

empty :: Env
empty = Env M.empty

-- Add var/value pairs to an environment.
extend :: [String] -> [V] -> Env -> Env
extend xs vs (Env m)
  | length xs == length vs =
      Env $ M.fromList (zip xs vs) `M.union` m
extend xs vs env = error $ "extend " ++ show (xs, vs, env)

envValue :: String -> Env -> V
envValue x (Env m) =
  case M.lookup x m of
    Just v -> v
    _ -> error $ "envValue: not found: " ++ x

-- Data for Scheme "built-in" operations. Each line is
-- (op-name, (number-of-arguments, f)) where f is a function mapping
-- input values (in V) to an output value.
primitives :: M.Map String (Int, [V] -> V)
primitives =
  M.fromList
    [ ("+", (2, \[VNumber x, VNumber y] -> VNumber (x + y))),
      ("*", (2, \[VNumber x, VNumber y] -> VNumber (x * y))),
      ("-", (2, \[VNumber x, VNumber y] -> VNumber (x - y))),
      ("eq?", (2, \[x, y] -> VBool $ x == y)),
      ("null?", (1, \[v] -> VBool (v == VNil))),
      ("cons", (2, \[x, y] -> VCons x y)),
      ("car", (1, \[VCons x _] -> x)),
      ("cdr", (1, \[VCons _ y] -> y))
    ]

specialForms = ["if", "let", "let*", "letrec", "lambda", "lambdaf"]

isPrimitive :: String -> Bool
isPrimitive x = M.member x primitives


-- run "(zip (cons 1 2) (cons 1 2))"






-- Evaluate an expression
eval :: Env -> Exp -> V

-- run "(test-letrec-0)"
-- eval env (List (Atom "letrec" : es) ) =
  -- trace ("\n LETREC n")
  -- trace (show es)
  -- VNil
  -- VFunc (evalApply VNil )



-- run "(test-letrec-0)"

-- VFunc ([V] -> V)
-- evalApply :: V -> [V] -> V

-- VFunc(_)
-- Valid hole fits include
--     head
--     last
--   Valid refinement hole fits include

--     applyPrimitive _
--     evalApply _

--     foldl1 _
--     foldr1 _
--     head _
--     last _

-- eval env (List (Atom "letrec" : es)) =
--   VFunc $ evalApply (map (eval env) es)

-- eval env (List (Atom "letrec" : es)) =
--   VFunc $ evalApply _

  -- in VFunc f
  -- trace ("\n ES: \n" ++ show es)
  -- trace ("\n ES: \n" ++ show (head es))
  -- trace ("\n env: \n" ++ show env)
  -- VFunc(evalApply (eval env (head es)))
  -- VFunc(applyPrimitive _)
  -- VNil







    -- List [Atom "define",List [Atom "test-string-case-0"],List [Atom "let",List [List [Atom "f",List [Atom "lambda",List [],String "bazola"]]],List [Atom "eq?",Number 2,List [Atom "string-case",List [Atom "f"],List [List [String "foo",List [Atom "+",Number 1,Number 17]],List [String "bar",List [Atom "+",Number 1,Number 17]],List [String "bazola",List [Atom "+",Number 1,Number 1]],List [String "gleep",List [Atom "+",Number 1,Number 17]]],List [Atom "+",Number 1,Number 41]]]]]


-- run "(test-string-case-0)"
-- List (String s : es)
-- eval env (List (String s : es) ) =
--   let
--     v = VString s
--   in
--     trace ("\n STRING: " ++ show env) v

eval env (List (String s : es) ) = VNil
  -- let
  --   v = VString s
  -- in
  --   -- trace ("\n STRING: " ++ show env) v



-- ghci> str <- readFile "a10.txt"
-- ghci> parseDefs str


-- List [Atom "let",List [List [Atom "x",Number 0],List [Atom "y",Number 1]],List [Atom "+",Atom "x",Atom "y"]]

-- List [List [Atom "x",Number 0],List [Atom "y",Number 1]]
-- List [Atom "+",Atom "x",Atom "y"]

-- extend :: [String] -> [V] -> Env -> Env
-- eval :: Env -> Exp -> V
-- evalApply :: V -> [V] -> V
-- run "(test-let-0)"
eval env (List (Atom "let" : es) ) =

  eval newEnv (es !! 1) -- cant be a list, perhaps recursion
  where
    list = es !! 0
    lb = letBindings list
    newEnv = extend (map fst lb) (map (eval env) (map snd lb)) env


  -- let
    -- list = es !! 0
    -- lb = letBindings list
  -- in
  --   trace ("\n Head ES: " ++ show list)
  --   trace ("\n LET BINDINGS: " ++ show lb) -- LET BINDINGS: [("x",Number 0),("y",Number 1)]
  --   trace ("\n map fst lb: " ++ show (map fst lb))
  --   trace ("\n map snd lb: " ++ show (map snd lb))

  -- where
  --   newEnv = extend (map fst lb) (map (eval env) (map snd lb)) env
    -- extend (extractFirst (map fst lb)) (extractSecond (es !! 0)) env

    -- evalApply (eval env (head es)) (map (eval env) (tail es))

-- need to add first binding to environment
-- need to add second binding to environment and evaluate with both bindings
-- second binding might have stuff the first binding to work


eval env (List (Atom "let*" : List bindings : body : [])) =
  eval newEnv body
  where
    newEnv = foldl extendBinding env bindings
    extendBinding e (List [Atom var, expr]) =
      extend [var] [eval e expr] e




-- closure is like a snapshot of the environment at the time the function was defined
-- closure is like a function where env has a closed context
-- time function defined everything in env at the time is all the function needs
-- closer is adding 17 to x, parameter x
-- closer are for function definitions, + x 17
-- closure is where you make the function definition
-- abstract express -> function can be applied

-- dont need to worry about eval apply, its like a lower level

-- need to just make closure

-- List [Atom "define",List [Atom "test-lambda-0"],List [Atom "eq?",Number 20,List [List [Atom "lambda",List [Atom "x"],List [Atom "+",Atom "x",Number 17]],Number 3]]]

-- List [Atom "lambda",List [Atom "x"],List [Atom "+",Atom "x",Number 17]],Number 3

-- run "(test-lambda-0)"
-- ghci> str <- readFile "a10.txt"
-- ghci> parseDefs str


-- evalApply :: V -> [V] -> V
          -- fnName,  env,    parms,   body
-- data V = VClosure String Env [String] Exp


-- dont have to pattern match like rest of the string, can use square bracket
eval env (List [Atom "lambda", List a, b])  =
  let
    list = a
    -- lb = letBindings list
    dv = VClosure "lambda" env (stringem a) b
    -- geter = envValue "x" env
    -- lk = envValue "x" env

  in
    -- trace ("\n geter: " ++ show geter)
    -- trace ("\n lookup x: " ++ show lk)
    -- trace ("\n a: " ++ show a)
    -- trace ("\n b: " ++ show b)

    -- trace ("\n LET BINDINGS: " ++ show lb) -- LET BINDINGS: [("x",Number 0),("y",Number 1)]
    -- trace ("\n map fst lb: " ++ show (map fst lb))
    -- trace ("\n map snd lb: " ++ show (map snd lb))
  -- VNil
  dv


eval env (List [Atom "lambdaf", List a, b])  =
  let
    list = a
    -- lb = letBindings list
    dv = VClosure "lambdaf" env (stringem a) b
    -- geter = envValue "x" env
    -- lk = envValue "x" env

  in
    -- trace ("\n geter: " ++ show geter)
    -- trace ("\n lookup x: " ++ show lk)
    -- trace ("\n a: " ++ show a)
    -- trace ("\n b: " ++ show b)

    -- trace ("\n LET BINDINGS: " ++ show lb) -- LET BINDINGS: [("x",Number 0),("y",Number 1)]
    -- trace ("\n map fst lb: " ++ show (map fst lb))
    -- trace ("\n map snd lb: " ++ show (map snd lb))
  -- VNil
  dv


-- ghci> str <- readFile "a10.txt"
-- ghci> parseDefs str


-- [[Atom "zip",Atom "l0",Atom "l1"],List [Atom "if",List [Atom "null?",Atom "l0"],List [Atom "list"],List [Atom "if",List [Atom "null?",Atom "l1"],List [Atom "list"],List [Atom "cons",List [Atom "cons",List [Atom "car",Atom "l0"],List [Atom "car",Atom "l1"]],List [Atom "zip",List [Atom "cdr",Atom "l0"],List [Atom "cdr",Atom "l1"]]]]]]

eval env (List (Atom "zip" : es) ) =

  let
    list = es
    -- lb = letBindings list
  in
    -- trace ("\n Head ES: " ++ show (head list))
  VNil




eval env (Atom x) =
  envValue x env
eval env (String s) =
  VString s
eval env (Bool b) =
  VBool b
eval env (Number n) =
  -- trace ("\n NUMBER N: " ++ show n)
  VNumber n
eval env Nil =
  VNil
eval env (List []) =
  VNil
eval env (List [Atom "list"]) =
  VNil
eval env (List (Atom "list" : es)) =
  foldr VCons VNil (map (eval env) es)
eval env (List [Atom "if", b, x, y])
  | eval env b == VBool True =
      eval env x
eval env (List [Atom "if", _, _, y]) =
  eval env y
eval env e@(List (Atom "if" : _)) =
  error $ "eval: malformed if " ++ show e

eval env (List (Atom f : args))
  | isPrimitive f =
      applyPrimitive f (map (eval env) args)
eval env (List (Atom f : args))
  | f `elem` specialForms =
      error $ "eval: special form " ++ f ++ " not implemented"
eval env (List (e : args)) =
  evalApply (eval env e) (map (eval env) args)

evalApply :: V -> [V] -> V
evalApply (VClosure fName fEnv vars body) vs
  | null fName =
      eval (extend vars vs fEnv) body
evalApply c@(VClosure fName fEnv vars body) vs =
  eval (extend (fName : vars) (c : vs) fEnv) body


letBindings :: Exp -> [(String, Exp)]
letBindings (List exps) =
  map (\(List [var, e]) -> (stringit var, e)) exps

stringCases :: Exp -> [(String, Exp)]
stringCases (List exps) =
  map (\(List [String s, e]) -> (s, e)) exps

stringit :: Exp -> String
stringit (Bool x) = show x
stringit (String x) = x
stringit (Number x) = show x
stringit (Atom x) = x
stringit _ = ""

stringem :: [Exp] -> [String]
stringem = map stringit

-- Apply a built-in to its arguments
applyPrimitive :: String -> [V] -> V
applyPrimitive f vs =
  let (arity, fValue) = fromJust (M.lookup f primitives)
   in if arity == length vs
        then fValue vs
        else error $ "applyPrimitive arity error in application of " ++ f

-- compileDefs p: compute an environment from the definitions in p and
compileDefs :: [Exp] -> Env
compileDefs defs =
  foldr addDef empty defs

-- Extend the environment using the definition.
addDef :: Exp -> Env -> Env
addDef (List [Atom "define", List (Atom fName : vars), body]) env =
  extend [fName] [VClosure fName env (map stringit vars) body] env

e = "(zip (list 1 2 3) (list 4 5 (factorial 6)))"

parseDefs :: String -> [Exp]
parseDefs str = programDefs $ parseProgram $ str ++ "\n(list)"

defsFromFile :: String -> IO [Exp]
defsFromFile fileName = do
  str <- readFile fileName
  return $ parseDefs str

run :: String -> IO V
run e = do
  str <- readFile "a10.txt"
  return $ eval (compileDefs (parseDefs str)) (parseExp e)


