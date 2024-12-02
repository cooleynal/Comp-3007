import Data.List (findIndex)
import Data.Maybe (fromJust, isJust, mapMaybe)
-------- DON'T TOUCH THE ABOVE
-- import Debug.Trace (trace)

-- This is a simplification of Assignment 10.
-- The main difference is that instead of using (List [Atom "f", Number 2, Number 3)
-- to represent "(f 2 3)" we're using (App "f" [Number 2, Number 3]). There is still
-- a List constructor, but it's only used for the arguments to "define".
-- As usual, fill in as indicated by TODO or "undefined".
--
-- You will be filling in the eval code for some new "special forms".

-- (mkSeq e0 e1 ... en)
-- evaluates to the sequences of values of e0, e1, ..., en. The type
-- V of values has been updated to include sequences.

-- (index e0 e1)
-- evaluates to the i-th value of the sequence that is the value of e0,
-- where i is the value of e1.

-- (delay e) is a value that represents a delayed computation. The value
-- of (delay e) is computed without evaluating e. It is a value that can
-- later be "forced", at which point the value of e is computed and returned.
-- The V type does not need extending for this. As an example, if we have
-- definition (define (f x) (f x)), then (delay (f 0)) immediately returns
-- a value, whereas (f 0), and hence (force (delay (f 0))), run forever.

-- (force e) is evaluated by first evaluating e, which must be the result
-- of a (delay e1) expression. The value of (force e) is the value of e1.

data Exp
  = Var String
  | Number Int
  | Bool Bool
  | List [Exp]
  | App String [Exp]
  deriving (Eq, Show)

data V
  = VNumber Int
  | VBool Bool
  | VSeq [V]  -- new for the quiz; see below
  | VClosure String Env [String] Exp -- fnName, env, parms, body
  deriving (Eq, Show)

-- As before, except use association lists instead of a Map
newtype Env = Env {envMap :: [(String, V)]} deriving (Show, Eq)

empty :: Env
empty = Env []

extend :: [String] -> [V] -> Env -> Env
extend xs vs (Env m)
  | length xs == length vs =
      Env $ zip xs vs ++ m
extend xs vs env = error $ "extend " ++ show (xs, vs, env)

envValue :: String -> Env -> V
envValue x (Env m) =
  case lookup x m of
    Just v -> v
    _ -> error $ "envValue: not found: " ++ x


-- Evaluate an expression
eval :: Env -> Exp -> V
eval env (Var x) =
  envValue x env
eval env (Bool b) =
  VBool b
eval env (Number n) =
  VNumber n
eval env (App "+" [e0, e1]) =
  case (eval env e0, eval env e1) of
    (VNumber n0, VNumber n1) -> VNumber (n0 + n1)
    _ -> error "eval +"
eval env (App "*" [e0, e1]) =
  case (eval env e0, eval env e1) of
    (VNumber n0, VNumber n1) -> VNumber (n0 * n1)
    _ -> error "eval +"
eval env (App "-" [e0, e1]) =
  case (eval env e0, eval env e1) of
    (VNumber n0, VNumber n1) -> VNumber (n0 - n1)
    _ -> error "eval +"
eval env (App "if" [b,e0,e1]) =
  if eval env b == VBool True then  eval env e0  else eval env e1
eval env (App "eq?" [e0,e1]) =
  VBool $ eval env e0 == eval env e1

-- change es to VSeq [V]
eval env (App "mkSeq" es) =
  -- trace ( "\nmkSeq:  " ++ show es)
  VSeq (map (eval env) es)


-- get index at in list
-- send expression to V or [V]?
-- run "(index (mkSeq 1 32 4) 0)"
-- eval env (App "index" [seqExp, iExp]) =
  -- trace ( "\nindex:  " ++ show seqExp)
  -- VNumber eval env (seqExp !! eval env (iExp) )
  -- VNumber eval env (seqExp !!  iExp)

  -- VNumber (eval env seqExp) !! iExp

  -- VNumber eval env (seqExp !! iExp)
--  envValue env (seqExp !! iExp)
  -- undefined


eval env (App "index" [seqExp, iExp]) =
  case (eval env seqExp, eval env iExp) of
    (VSeq seq, VNumber i) ->
      if i >= 0 && i < length seq
        then seq !! i
        else error "index: out of bounds"
    _ -> error "index: invalid arguments"



-- run "(force (delay 2))"
-- VNumber 2
-- eval env (App "delay" [e]) =
--   -- VNumber (eval env e)
--   undefined

eval env (App "delay" [e]) =
  VClosure "delay" env [] e



eval env (App f es) =
  let c@(VClosure fName fEnv vars body) = envValue f env in
  eval (extend (fName : vars) (c : (map (eval env) es)) fEnv) body


-- NOT USED
-- compileDefs p: compute an environment from the definitions in p and
compileDefs :: [Exp] -> Env
compileDefs defs =
  foldr addDef empty defs

  -- NOT USED
-- Extend the environment using the definition.
addDef :: Exp -> Env -> Env
addDef (App "define" [List (Var fName : vars), body]) env =
  extend [fName] [VClosure fName env (stringem vars) body] env
addDef e _ = error $ "addDef: " ++ show e

-- NOT USED
defsFromFile :: String -> IO [Exp]
defsFromFile fileName = do
  str <- readFile fileName
  let List es = parseList ("(" ++ str ++ ")")
  return es

-- NOT USED
runWithDefs :: [String] -> String -> V
runWithDefs defs e = do
  let initEnv = compileDefs (map parseExp defs)
  eval initEnv (parseExp e)

-- Use this for testing
run :: String -> V
run str = eval empty (parseExp str)

-- Examples
-- ghci> run "(mkSeq 1 32 4)"
-- VSeq [VNumber 1,VNumber 32,VNumber 4]
-- ghci> run "(index (mkSeq 1 32 4) 0)"
-- VNumber 1
-- ghci> run "(force (delay 2))"
-- VNumber 2

------------------------------------------------------
-- Parsing below. Nothing to see there. Move along. --
------------------------------------------------------

schemeWords :: String -> [String]
schemeWords [] = []
schemeWords (c:cs) | c == '(' =
  let (parend, rest) = splitAtCloser (c:cs) in
    parend : schemeWords (dropWhile (== ' ') rest)
schemeWords s =
  take 1 (words s) ++ schemeWords (unwords (drop 1 (words s)))

splitAtCloser :: String -> (String, String)
splitAtCloser ('(':s) =
  let rest = afterMatch 1 s in
    (take (length ('(':s) - length rest) ('(':s), rest)
  where
    afterMatch n ('(':s) = afterMatch (n+1) s
    afterMatch n (')':s) = afterMatch (n-1) s
    afterMatch 0 s = s
    afterMatch n (_:s) = afterMatch n s
    afterMatch _ "" = ""
splitAtCloser _ = error "splitAtCloser"

-- Assuming the entire string is parenthesized
stripParens :: String -> String
stripParens s =
    drop 1 (take (length s - 1) s)

-- function names and variables
isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (c:cs) =
  c `elem` "*+@&^$#!?-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUV"
  && all (`elem` "*+@&^$#!?-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUV0123456789") cs

parseExp :: String -> Exp
parseExp [] = error "parseExp: empty string"
parseExp s@('(':_) = parseForm (schemeWords (stripParens s))
parseExp s | all (`elem` "0123456789") s = Number (read s)
parseExp "#f" = Bool False
parseExp "#t" = Bool True
parseExp s | isIdentifier s = Var s
parseExp s = error $ "parseExp: " ++ s

parseForm :: [String] -> Exp
parseForm [] = error "parseForm: empty string"
parseForm ["define", vars, body] =
  App "define" [parseList vars, parseExp body]
parseForm (f:ws) | isIdentifier f = App f (map parseExp ws)
parseForm s = error $ "parseForm" ++ show s

parseList :: String -> Exp
parseList s = List (map parseExp (schemeWords (stripParens s)))

stringit :: Exp -> String
stringit (Bool x) = show x
stringit (Number x) = show x
stringit (Var x) = x
stringit _ = ""

stringem :: [Exp] -> [String]
stringem = map stringit
