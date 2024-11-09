{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- import Debug.Trace (trace)
-- Check out the imported functions. You might find them useful. Also, take a
-- look at the bottom of the file for some useful general utilities.
import Data.List (findIndex, nub)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Text (pack, unpack) -- needed but you can ignore them
import Debug.Trace
import Syntax qualified as S
import Text.Pretty.Simple (pPrint) -- you can delete this line, but pPrint is handy for debuggine

-------- DON'T TOUCH ANYTHING ON THIS LINE OR ABOVE ------------

-----------------------------------------------------------------
-- Scheme abstract syntax
-- Nothing for you to do here, but you will need to know it all.
-----------------------------------------------------------------

myunsnoc :: [a] -> Maybe ([a], a)
myunsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

-- ASTs for Scheme expressions
data Exp
  = Atom String
  | List [Exp]
  | Number Int
  | String String
  | Nil
  | Bool Bool
  deriving (Eq, Ord, Show)

data Program = Program
  { programDefs :: [Exp],
    programExp :: Exp
  }
  deriving (Show, Eq)

runProgram :: Program -> String
runProgram prog = unparseExp $ reduce (builtinRules ++ programRules prog) (programExp prog)

runStepper :: Program -> IO ()
runStepper prog = reduceStepwise (builtinRules ++ programRules prog) (programExp prog)

-- isValue e: e cannot be further evaluated. Note: this function defines a
-- set of *expressions* that we are considering to be values. All the constants
-- are values. The expression List [] (as a string: "(list)") represents "nil",
-- or the empty list. All other data is built using cons. An expression List
-- (Atom "cons) e0 e1 is a value exactly if the expressions e0 and e1 are.
isValue :: Exp -> Bool
isValue (Number _) = True
isValue (String _) = True
isValue (Bool _) = True
isValue Nil = True
isValue (List [Atom "cons", e0, e1]) = isValue e0 && isValue e1
isValue (List [Atom "list"]) = True
isValue _ = False

-- This function uses the Syntax module. You can ignore how it's implemented, but
-- note that it checks that the expression is "well-formed". See the definition of
-- isWFExp.
parseExp :: String -> Exp
parseExp str = case S.readExpr (pack str) of
  Right v ->
    let e = convert v
     in assert isWFExp ("parseExp: expression not well formed: " ++ unparseExp e) e
  Left e -> error $ show e

-- A Scheme program is a sequence of definitions followed by a single expression to
-- evaluate using the defined functions.
parseProgram :: String -> Program
parseProgram str =
  let prog@(Program defs e) = parseRawProgram str
      msg = "parsePprogram not well-formed: " ++ concatMap unparseExp defs ++ unparseExp e
   in assert isWFProgram msg (Program defs e)

-- The ASTs but without doing the well-formedness check (isWfProgram)
parseRawProgram :: String -> Program
parseRawProgram str =
  let List es' = parseExp $ "(list " ++ str ++ ")"
      es = tl es'
   in if null es
        then
          error "parseProgram: empty program"
        else
          let (defs, e) = fromJust (myunsnoc es)
           in Program defs e

-- All non-empty List objects must start with an atom. E.g. "(cons 1 2)" is well-formed,
-- but ((add1 1) 2) is not since the first element is "(add1 1)".
isWFExp :: Exp -> Bool
isWFExp (List (Atom _ : es)) | length es <= 4 = True
isWFExp (List (_ : _)) = False
isWFExp _ = True

-- All the elements of "defs" are definitions, i.e. of the form "(define (f x y ...) ...)".
-- See isDef for details. The final expression, e, cannot be a definition.
isWFProgram :: Program -> Bool
isWFProgram (Program defs e) =
  all isDef defs && not (isDef e)

isDef :: Exp -> Bool
isDef (List [Atom "define", List (Atom f : vars), e]) =
  all isAtom vars && noDuplicates vars
isDef _ = False

isAtom :: Exp -> Bool
isAtom (Atom _) = True
isAtom _ = False

isList :: Exp -> Bool
isList (List _) = True
isList _ = False

isBool :: Exp -> Bool
isBool (Bool _) = True
isBool _ = False

isNumber :: Exp -> Bool
isNumber (Number _) = True
isNumber _ = False

isString :: Exp -> Bool
isString (String _) = True
isString _ = False

-- Inverse of parse: a string whose parse succeeds and returns the given expression.
unparseExp :: Exp -> String
unparseExp (Atom str) = str
unparseExp (List es) = "(" ++ unwords (map unparseExp es) ++ ")"
unparseExp (Number n) = show n
unparseExp (String str) = show str
unparseExp Nil = "'()"
unparseExp (Bool True) = "#t"
unparseExp (Bool False) = "#f"

unparseProgram :: Program -> String
unparseProgram (Program defs e) = concatMap unparseExp defs ++ unparseExp e

-- Convert the Syntax module's ASTs to Exps.
instance Convert S.LispVal Exp where
  convert (S.Atom txt) = Atom (unpack txt)
  convert (S.List es) = List $ map convert es
  convert (S.Number n) = Number (fromInteger n)
  convert (S.String txt) = String (unpack txt)
  convert S.Nil = Nil
  convert (S.Bool b) = Bool b

instance Convert Exp Int where
  convert (Number x) = x
  convert e = error $ "can't convert to number: " ++ show e

instance Convert Exp Bool where
  convert (Bool x) = x
  convert e = error $ "can't convert to boolean: " ++ show e

instance Convert Exp String where
  convert (String x) = x
  convert e = error $ "can't convert to string: " ++ show e

----------------------------------
-- Lenses ------------------------
-- -------------------------------

-- A path is a sequence of non-negative integers.
type Path = [Int]

-- A lens is an expression together with a path that is valid for the expression.
-- Valid means the path can be followed to its end in the expression. What "following" a
-- path means is given by the definition of the function "get".
data Lens = Lens
  { lensExp :: Exp,
    lensPath :: Path
  }
  deriving (Show, Eq)

get :: Lens -> Exp
get (Lens e []) = e
get (Lens (List es) (k : ks))
  | 0 <= k && k < length es =
      get (Lens (es !! k) ks)
get (Lens e ks) = error $ "getUsingPath: " ++ show ks ++ show e

set :: Lens -> Exp -> Lens
set l e0 = Lens (set' l e0) (lensPath l)

set' :: Lens -> Exp -> Exp
set' (Lens e []) e0 = e0
set' (Lens (List es) (k : ks)) e0
  | 0 <= k && k < length es =
      List $ replaceNth k (set' (Lens (es !! k) ks) e0) es
set' (Lens e ks) e0 = error $ "getUsingPath: " ++ show ks ++ show e

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x xs = take n xs ++ [x] ++ drop (n + 1) xs

-- findSubexp :: (Exp -> Bool) -> Exp -> Maybe Lens
-- findSubexp = undefined



findSubexp :: (Exp -> Bool) -> Exp -> Maybe Lens
findSubexp f exp = findLens exp exp []
  where
    findLens :: Exp -> Exp -> Path -> Maybe Lens
    findLens original e path
      | f e = Just (Lens original path)
      | otherwise = case e of
          List elements -> 
            foldl (\acc (subExp, idx) -> 
                    case acc of
                      Just lens -> Just lens
                      Nothing -> findLens original subExp (path ++ [idx])
                  ) Nothing (zip elements [0..])
          Atom _ -> 
            if f e then Just (Lens original path) else Nothing
          _ -> Nothing
          


-- Produce a string for the expression as in unparseExp, but enclose the
-- expression addressed by the path with "[[" and "]]".
unparseLens :: Lens -> String
unparseLens (Lens e []) = "[[" ++ unparseExp e ++ "]]"
unparseLens (Lens (List es) (k : ks))
  | 0 <= k && k < length es =
      (\elements -> "(" ++ unwords elements ++ ")") $
        map unparseExp (take k es) ++ [unparseLens (Lens (es !! k) ks)] ++ map unparseExp (drop (k + 1) es)
unparseLens (Lens e _) = unparseExp e

----------------------------------------------
-- Substitution ------------------------------
----------------------------------------------

-- "Environments" are just another form of map, similar to
-- data Env k = [(k,a)], but using the Map library (for no good
-- reason). You can think of "newtype" as being the same as "data".
newtype Env a = Env {envMap :: M.Map String a} deriving (Show)

-- Combine two environments.
merge :: Env a -> Env a -> Env a
merge (Env m0) (Env m1) = Env $ M.union m0 m1

empty :: Env a
empty = Env M.empty

-- Add a key/value pair to an environment.
extend :: String -> a -> Env a -> Env a
extend x y env = Env $ M.insert x y (envMap env)

lookup :: String -> Env a -> Maybe a
lookup e (Env m) = M.lookup e m

findWithDefault :: a -> String -> Env a -> a
findWithDefault v x = M.findWithDefault v x . envMap

type Subst = Env Exp

-- subst s e: for each key/value pair (x,e0) in s, replace all occurrences
-- of (Atom x) in e by e0.
subst :: Subst -> Exp -> Exp
subst s (List (f : es)) = List $ f : map (subst s) es
subst s e@(Atom str) = findWithDefault e str s
subst s e = e

-- Syntactic sugar for applying a substitution to an atom.
(<@>) :: Subst -> String -> Exp
s <@> x = subst s (Atom x)

--------------------------------------------
-- Rewriting/reduction rules ---------------
--------------------------------------------

-- To apply a rule to an expression e:
-- 1. Find a substitution s that gives e when applied to ruleLhs.
-- 2. If ruleRhs is an expression e', apply s to it. If it is a function,
--    apply the function to e'.
data Rule = Rule
  { ruleLhs :: Exp,
    ruleRhs :: Either Exp (Subst -> Exp)
  }

-- Do a half-assed job of showing a rule since the function part can't be
-- shown.
instance Show Rule where
  show (Rule e0 (Left e1)) =
    show "Rule " ++ "(" ++ show e0 ++ show ")" ++ "(" ++ show e1 ++ show ")"
  show (Rule e0 _) = show e0

-- Find the subexpression that should be reduced next. The general idea is to
-- start at the top to look for something to reduce. If it's a function call
-- whose arguments are all values (see isValue) then the whole expression can
-- be reduced by replacing the function call by the function body with the
-- value arguments plugged in. If not, one of the arguments must not be value.
-- Start searching from the first such argument. Note that the entire path from
-- the top of the original expression to redex needs to be collected.
-- E.g.
-- nextRedex (parseExp "(cons (list) (cons (+ 1 2) (list)))"
-- = Just (Lens (...) [2,1])
-- since the first argument to the leftmost "cons" is "(list)", which we have
-- defined as the value representing the empty list. [2] addresses the second
-- cons expression, and so [2,1] addresses "(+ 1 2)", which is a redex since
-- it is not a value but is a function applied to value arguments.
-- nextRedex :: Exp -> Maybe Lens
-- nextRedex = undefined



-- passes test but breaks runProgram

nextRedex :: Exp -> Maybe Lens
nextRedex e = findRedex e []
  where
    findRedex :: Exp -> Path -> Maybe Lens
    findRedex (List (f : args)) path
      | all isValue args = Just (Lens e path)
      | otherwise = findInArgs args path 0
    findRedex _ _ = Nothing

    findInArgs :: [Exp] -> Path -> Int -> Maybe Lens
    findInArgs [] _ _ = Nothing
    findInArgs (arg : rest) path index
      | not (isValue arg) = findRedex arg (path ++ [index + 1])  
      | otherwise = findInArgs rest path (index + 1)



-- A "standard" rule is a rule whose ruleRhs is an expression. The function
-- takes strings as arguments and parses them.
mkStdRule :: String -> String -> Rule
mkStdRule lhs rhs =
  Rule {ruleLhs = parseExp lhs, ruleRhs = Left (parseExp rhs)}

-- A 'primitive" rule is a rule whose ruleRhs is a function. It's called
-- "primitive" since it's needed for reductions that need Haskell primitives,
-- like addition, and can't be defined just by pattern matching and
-- substitution.
mkPrimitiveRule :: String -> (Subst -> Exp) -> Rule
mkPrimitiveRule str f =
  Rule {ruleLhs = parseExp str, ruleRhs = Right f}
  where
    List [e0, e1] = parseExp str

programRules :: Program -> [Rule]
programRules = map compileDef . programDefs

compileDef :: Exp -> Rule
compileDef (List [Atom "define", lhs@(List (Atom f : vars)), e]) =
  Rule {ruleLhs = lhs, ruleRhs = Left e}
compileDef e = error $ "compileDef: not a def " ++ show e

-- matchPat lhs rhs: find a substitution s such that subst lhs = rhs.
-- matchPat :: Exp -> Exp -> Maybe Subst
-- matchPat = undefined

matchPat :: Exp -> Exp -> Maybe Subst
matchPat (Atom var) rhs = Just (extend var rhs empty)
matchPat (Number n1) (Number n2)
  | n1 == n2  = Just empty
matchPat (String n1) (String n2)
  | n1 == n2  = Just empty
matchPat (Bool n1) (Bool n2)
  | n1 == n2  = Just empty
matchPat Nil Nil = Just empty
matchPat (List (Atom f : argsLhs)) (List (Atom f' : argsRhs))
  | f == f' = matchPats argsLhs argsRhs
matchPat _ _ = Nothing






-- matchPats lhss rhss: find a substitution s such that subst lhs = rhs for each
-- corresponding pair of elements in lhss and rhss.
-- matchPats :: [Exp] -> [Exp] -> Maybe Subst
-- matchPats = undefined

matchPats :: [Exp] -> [Exp] -> Maybe Subst
matchPats [] [] = Just empty
matchPats (x:xs) (y:ys) = do
  s1 <- matchPat x y
  let xs' = map (subst s1) xs
      ys' = map (subst s1) ys
  s2 <- matchPats xs' ys'
  return (merge s1 s2)
matchPats _ _ = Nothing




-- Try to match e to the rule's lhs and return the result of applying the matching
-- substitution, if one was found, to the rhs.
applyRule :: Exp -> Rule -> Maybe Exp
applyRule e r = do
  s <- matchPat (ruleLhs r) e
  case ruleRhs r of
    Left e' -> return (subst s e')
    Right f -> return (f s)

-- Find the first rule that succeeds on the addressed subexpression and replace the
-- subexpression by the reduced/rewritten term.
reduce1 :: [Rule] -> Lens -> Maybe Exp
reduce1 rules lens = do
  firstSuccess <- hd $ mapMaybe (applyRule (get lens)) rules
  return $ lensExp (set lens firstSuccess)

-- Repeatedly find the nextRedex and reduce1 it, repeating until there are is
-- no nextRedex.
reduce :: [Rule] -> Exp -> Exp
reduce rules e =
  case step of
    Just e' -> reduce rules e'
    Nothing -> e
  where
    step = do
      lens <- nextRedex e
      reduce1 rules lens

-- Like reduce, but interactive.
reduceStepwise :: [Rule] -> Exp -> IO ()
reduceStepwise rules e = do
  case step of
    Just (lens, e') -> do
      putStrLn $ unparseLens lens
      putStrLn "REDUCES TO: "
      putStrLn $ unparseLens (Lens e' (lensPath lens))
      putStr "CONTINUE? [Hit Enter]"
      getLine
      reduceStepwise rules e'
    Nothing ->
      return ()
  where
    step = do
      lens <- nextRedex e
      e' <- reduce1 rules lens
      return (lens, e')

-- Rules for Scheme "built in" operations.
builtinRules :: [Rule]
builtinRules =
  [ mkStdRule "(if #t x y)" "x",
    mkStdRule "(if #f x y)" "y",
    mkPrimitiveRule
      "(+ x y)"
      (\s -> Number (convert (s <@> "x") + convert (s <@> "y"))),
    mkPrimitiveRule
      "(* x y)"
      (\s -> Number (convert (s <@> "x") * convert (s <@> "y"))),
    mkPrimitiveRule
      "(- x y)"
      (\s -> Number (convert (s <@> "x") - convert (s <@> "y"))),
    mkStdRule "(list x)" "(cons x (list))",
    mkStdRule "(list x y)" "(cons x (cons y (list)))",
    mkStdRule "(list x y z)" "(cons x (cons y (cons z (list))))",
    mkStdRule "(list x y z a)" "(cons x (cons y (cons z (cons a (list)))))",
    mkStdRule "(car (cons x y))" "x",
    mkStdRule "(cdr (cons x y))" "y",
    Rule
      (List [Atom "boolean?", Atom "x"])
      (Right (Bool . isBool . (<@> "x"))),
    Rule
      (List [Atom "number?", Atom "x"])
      (Right (Bool . isNumber . (<@> "x"))),
    Rule
      (List [Atom "string?", Atom "x"])
      (Right (Bool . isString . (<@> "x"))),
    Rule
      (List [Atom "null?", Atom "x"])
      ( Right $ \s ->
          case s <@> "x" of
            List [Atom "list"] -> Bool True
            Nil -> Bool True
            _ -> Bool False
      ),
    Rule (List [Atom "null?", Nil]) (Left (Bool True)),
    Rule (List [Atom "null?", Atom "x"]) (Left (Bool False)),
    Rule (List [Atom "pair?", (List [Atom "cons", Atom "x", Atom "y"])]) (Left (Bool True)),
    Rule (List [Atom "pair?", Atom "x"]) (Left (Bool False)),
    Rule (List [Atom "eq?", Atom "x", Atom "y"]) $ Right $ \s ->
      Bool $ areEq (s <@> "x") (s <@> "y")
  ]

areEq :: Exp -> Exp -> Bool
areEq x y = not (isList x) && not (isList y) && x == y

----------------------------------------------------
-- Assorted general utilities and some sample data.
----------------------------------------------------

class Convert a b where
  convert :: a -> b

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates l = length l == length (nub l)

failingIf :: Bool -> Maybe a -> Maybe a
failingIf b x = if b then Nothing else x

failingUnless :: Bool -> Maybe a -> Maybe a
failingUnless b x = if b then x else Nothing

-- Check if an object satisfies a predicate, returning the object if it does,
-- else raising an error with the given message.
assert :: (a -> Bool) -> String -> a -> a
assert p _ x | p x = x
assert _ msg x = error msg

hd :: [a] -> Maybe a
hd (a : _) = Just a
hd _ = Nothing

tl :: [a] -> [a]
tl (x : l) = l
tl [] = []

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
      "(zip (list 1 2 3) (list 4 5 6))"
    ]

pExp = programExp p

pRules = map compileDef (programDefs p) ++ builtinRules



snoc :: Program
snoc =
  (parseProgram . unlines)
    [ "(define (snoc x l)"
    , "  (if (null? l)"
    , "      (list x)"
    , "      (cons (car l) (snoc x (cdr l)))))"
    , ""
    , "(snoc 17 (list #t 1 \"bazola\"))"
    ]

snocResult = "(cons #t (cons 1 (cons \"bazola\" (cons 17 (list)))))"

cons :: Exp -> Exp -> Exp
cons x y = List [Atom "cons", x, y]

app :: String -> [Exp] -> Exp
app f args = List (Atom f : args)

tree0 :: Exp
tree0 =
  cons
    (cons (cons (Number 1) (Number 2)) (cons (Number 3) (Number 4)))
    (cons (cons (Number 5) (Number 6)) (cons (Number 7) (Number 8)))

tree1 :: Exp
tree1 = lensExp $ set (Lens tree0 [2, 2, 1]) (app "+" [Number 1, app "+" [Number 2, Number 3]])


-- given code has name collisions with test code ...


main :: IO ()
main = do

  -- q1
  print $ "Q1"
  putStrLn ""
  print $ findSubexp (== Number 7) tree0
  -- print $ findSubexp (== (Atom "cons"))
  putStrLn ""
  putStrLn ""
  print $ findSubexp (== (Atom "cons")) tree0
  putStrLn ""
  putStrLn ""
  -- q2
  print $ "Q2"
  -- putStrLn "nextRedex lensExp $ set (Lens tree0 [2, 2, 1]) (app \"+\" [Number 1, app \"+\" [Number 2, Number 3]])"
  print $ nextRedex tree1
  putStrLn ""

  print $ nextRedex (parseExp "(cons (list) (cons (+ 1 2) (list)))")
  putStrLn "Just (Lens (...) [2,1])"
  putStrLn ""



--   print $ envMap (fromJust (matchPat pat0 inst0))

  -- print $ envMap (fromJust (matchPat pat0 inst0))


-- q5
  putStrLn "Q5"
  putStrLn "runProgram snoc"
  print $ runProgram snoc
  -- print $ "(cons #t (cons 1 (cons \"bazola\" (cons 17 (list)))))"
  putStrLn "should be: (cons #t (cons 1 (cons \"bazola\" (cons 17 (list)))))"
  pPrint tree0

  -- runProgram snoc gives ""(if #f (cons 17 (list)) (cons #t (if #f (cons 17 (list)) (cons 1 (if #f (cons 17 (list)) (cons \"bazola\" (if #t (cons 17 (list)) (cons (car (list)) (snoc 17 (cdr (list)))))))))))"" 

  -- THIS IS WRONG DO THIS 
  -- "(cons #t (cons 1 (cons \"bazola\" (cons 17 (list)))))"


