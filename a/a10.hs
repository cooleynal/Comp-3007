{-# LANGUAGE ImportQualifiedPost #-}

module SchemeEval where

import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)

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


-- run "(test-letrec-0)"

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

-- primitives :: M.Map String (Int, [V] -> V)
-- primitives =
--   M.fromList
--     [ ("+", (2, \[VNumber x, VNumber y] -> VNumber (x + y))),
--       ("*", (2, \[VNumber x, VNumber y] -> VNumber (x * y))),
--       ("-", (2, \[VNumber x, VNumber y] -> VNumber (x - y))),
--       ("eq?", (2, \[x, y] -> VBool $ x == y)),
--       ("null?", (1, \[v] -> VBool (v == VNil))),
--       ("cons", (2, \[x, y] -> VCons x y)),
--       ("car", (1, \[VCons x _] -> x)),
--       ("cdr", (1, \[VCons _ y] -> y)),
--       ("letrec", (1, \[VCons _ y] -> y))
--     ]



isPrimitive :: String -> Bool
isPrimitive x = M.member x primitives

-- run "(test-letrec-0)"
-- run "factorial n"

-- letrec
extendRecursive :: Env -> Exp -> Env
extendRecursive env (List bindings) =
  let
      makeClosure :: Exp -> (String, V)
      makeClosure (List [Atom fName, funcDef]) =
        let closure = VClosure fName env [] funcDef
        in (fName, closure)
      makeClosure badBinding =
        error $ "Invalid binding in letrec: " ++ show badBinding

      extendedBindings = map makeClosure bindings
  in extend (map fst extendedBindings) (map snd extendedBindings) env




eval :: Env -> Exp -> V

eval env (List (Atom "letrec" : es) ) =
  trace ("\n LETREC n")
  trace (show es)
  -- VNil
  VFunc (evalApply VNil )
  -- VFunc (evalApply (eval env l1) )

-- run "(test-letrec-0)"



-- eval env (List (Atom "letrec" : bindings : body)) =
--   let
--     -- Ensure bindings is a list of recursive function definitions
--     makeRecClosure :: Exp -> (String, V)
--     makeRecClosure (List [Atom fName, funcDef]) =
--       let closure = VFunc (\args -> eval (extend [fName] [VFunc (\_ -> VNil)] env) funcDef)
--       in (fName, closure)
--     makeRecClosure badBinding = error $ "Invalid binding in letrec: " ++ show badBinding

--     -- Create the recursive environment
--     extendedBindings = map makeRecClosure bindings
--     recursiveEnv = extend (map fst extendedBindings) (map snd extendedBindings) env
--   in
--     -- Evaluate the body in the recursive environment
--     foldl (\_ b -> eval recursiveEnv b) VNil body



-- run "test-let-0"
eval env (List (Atom "let" : xs)) = -- doesnt match.
  trace ("\nLet\n")
  VNil




eval env (Atom x) =
  -- trace ("\n eval env (Atom x): \n" ++ show env ++ "\n(Atom x): \n" ++ x)
  envValue x env
eval env (String s) =
  VString s
eval env (Bool b) =
  VBool b
eval env (Number n) =
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



eval env (List (e : args)) =
  -- trace ("\n ENV: \n" ++ show env)
  -- trace ("\n E: \n" ++ show e)
  -- trace ("\n ARGS: \n" ++ show args)

  -- trace ("\n (eval env e): \n" ++ show (eval env e))
  -- trace ("\n (map (eval env) args): \n" ++ show (map (eval env) args))

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

-- run "(test-letrec-0)"

run :: String -> IO V
run e = do
  str <- readFile "a10.txt"
  -- print (parseDefs str)
  -- print (compileDefs(parseDefs str))
  -- print ( (parseExp e) )
  pPrint (parseDefs str) -- forget how we isolated
  return $ eval (compileDefs (parseDefs str)) (parseExp e)




-- run2 "(test-letrec-0)"

-- addLet :: Env -> String -> Env
-- addLet (Env _) = (Env {envMap = fromList [("factorial","#"),("test-lambda-0","#"),("test-lambda-1","#"),("test-lambdaf-0","#"),("test-lambdaf-1","#"),("test-let*-0","#"),("test-let*-1","#"),("test-let-0","#"),("test-let-1","#"),("test-letrec-0","#"),("test-letrec-1","#"),("test-string-case-0","#"),("zip","#"),("letrec","#")
-- ]})


-- (Env {envMap = fromList [(\"factorial\",#),(\"test-lambda-0\",#),(\"test-lambda-1\",#),(\"test-lambdaf-0\",#),(\"test-lambdaf-1\",#),(\"test-let*-0\",#),(\"test-let*-1\",#),(\"test-let-0\",#),(\"test-let-1\",#),(\"test-letrec-0\",#),(\"test-letrec-1\",#),(\"test-string-case-0\",#),(\"zip\",#),(\"letrec\",#)]})


-- (Env {envMap = fromList [("factorial",#),("test-lambda-0",#),("test-lambda-1",#),("test-lambdaf-0",#),("test-lambdaf-1",#),("test-let*-0",#),("test-let*-1",#),("test-let-0",#),("test-let-1",#),("test-letrec-0",#),("test-letrec-1",#),("test-string-case-0",#),("zip",#),("letrec",#)
-- ]})

-- addLet (compileDefs(parseDefs (readFile "a10.txt")))
-- addLet (compileDefs(parseDefs str))

-- ghci> str <- readFile "a10.txt"
-- ghci> addLet (compileDefs(parseDefs str))


eval2 :: Env -> Exp -> V
-- eval2 env (Atom x) =
--   envValue x env
-- eval2 env (List [Atom "list"]) =
--   VNil
-- eval2 env (List []) =
--   VNil
-- eval2 env (List (Atom "list" : es)) =
--   foldr VCons VNil (map (eval env) es)
-- eval2 env (List (Atom f : args))
--   | isPrimitive f =
--       applyPrimitive f (map (eval env) args)

eval2 env (List (e : args)) =
  evalApply (eval env e) (map (eval env) args)

-- print (parseDefs str)
-- List [Atom "define",List [Atom "test-letrec-0"],List [Atom "eq?",Number 24,List [Atom "letrec",List [List [Atom "f",Atom "n"],List [Atom "if",List [Atom "eq?",Atom "n",Number 0],Number 1,List [Atom "*",Atom "n",List [Atom "f",List [Atom "-",Atom "n",Number 1]]]]],List [Atom "f",Number 4]]]]



-- ENV
-- print (compileDefs(parseDefs str))
-- Env {envMap = fromList [("factorial",#),("test-lambda-0",#),("test-lambda-1",#),("test-lambdaf-0",#),("test-lambdaf-1",#),("test-let*-0",#),("test-let*-1",#),("test-let-0",#),("test-let-1",#),("test-letrec-0",#),("test-letrec-1",#),("test-string-case-0",#),("zip",#)]}



