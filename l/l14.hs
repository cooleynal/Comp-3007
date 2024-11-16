{-# LANGUAGE ImportQualifiedPost #-}

module SchemeEval where

import Data.List (intercalate, nub)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import SchemeSyntax

-------- DON'T TOUCH ANYTHING ON THIS LINE OR ABOVE -----------------

-------- DON'T CHANGE THE NAME OF THIS FILE -------------------------

---------------------------------------------------------------------
-- An interpreter for Scheme in the style of denotational semantics.
-- For Scheme syntax, see the imported SchemeSyntax module
---------------------------------------------------------------------

-- The type of values that can result from evaluating a Scheme program.
data V
  = VNumber Int
  | VString String
  | VBool Bool
  | VNil
  | VCons V V
  | VPrimitive String
  | VClosure Env [String] Exp
  deriving (Eq)

-- E.g. the value of "(cons 1 2)" shows as "(1 . 2)". This is the
-- usual form in Lisp-related languages.
instance Show V where
  show (VNumber n) = show n
  show (VString s) = show s
  show (VBool b) = show b
  show (VNil) = "[]"
  show (VCons v0 v1) = "(" ++ show v0 ++ " . " ++ show v1 ++ ")"
  show (VPrimitive str) = "#"
  show (VClosure _ _ _) = "#"

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

isPrimitive :: String -> Bool
isPrimitive x = M.member x primitives

eval :: Env -> Exp -> V
eval = undefined

evalProgram :: Program -> V
evalProgram = undefined

p =
  (parseProgram . unlines)
    [ "(define (factorial n) ",
      "  (if (eq? n 0) ",
      "  1 ",
      "  (* n (factorial (- n 1)))))",
      "",
      "(define (zip l0 l1)",
      " (if (null? l0) ",
      "    (list)",
      "     (if (null? l1)",
      "         (list)",
      "         (cons (cons (car l0) (car l1)) (zip (cdr l0) (cdr l1))))))",
      "",
      "(zip (list 1 2 3) (list 4 5 (factorial 6)))"
    ]
