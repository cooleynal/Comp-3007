-- Quiz 3

-- Quiz format is as usual
--   - submit to Gradescope
--   - no questions during the test unless they're about logistics, e.g. CoMaS

-- Quiz length: by design this quiz is on the long side. Grade adjustments
-- might be made afterwards if the quiz is too long. If so, no one's grade will
-- go down. There are 8 questions but for most of them amount of code you need
-- to write is tiny.

-- Extra functions: you don't need any libraries or even any Prelude functions
-- beyond the usual ones like map etc, except perhaps for

--   any :: (a -> Bool) -> [a] -> Bool
--   all :: (a -> Bool) -> [a] -> Bool

-- You will likely find useful several functions provided at the bottom of this
-- file under "Utilities". **Keep these in mind especially in the later
-- questions.**

-- Remember that "do" is your friend when writing code involving the Maybe
-- type. It can sometimes eliminate a huge steaming pile of code clutter.

-- The definitions below that start with "assert" are the exact checks the
-- autograder will be using. TAs will be examining your code later to make sure
-- that your functions are not just e.g. returning the single expected output
-- regardless of the input.

-- Types that have a tree-like accessor, constructor and info field. A leaf is
-- a tree with no children.
class (Eq a) => Treeish a where
  children :: a -> [a]
  info :: a -> String
  build :: String -> [a] -> Maybe a

-- A most-general (archetypal?) member of the Treeish type class.
data Archetree
  = Node String [Archetree]
  deriving (Eq, Show)

-- QUESTION 1
instance Treeish Archetree where
  children = undefined
  info = undefined
  build = undefined

assertArcheTree =
  children at == [at0, at1, at2]
    && info at == "meh"
    && build "meh" [at0, at1, at2] == Just at
  where
    at0 = Node "0" []
    at1 = Node "1" []
    at2 = Node "2" []
    at = Node "meh" [at0, at1, at2]

-- Bog-standard binary trees with keys/info in the leaves
data BT = BTLeaf String | BTNode BT BT
  deriving (Eq, Show)

-- QUESTION 2
instance Treeish BT where
  children = undefined
  info = undefined
  build = undefined

assertBT =
  children bt == [bt0, bt1]
    && info bt1 == "1"
    && build "" [bt0, bt1] == Just bt
    && build "" [bt0] == Nothing
  where
    bt0 = BTLeaf "0"
    bt1 = BTLeaf "1"
    bt = BTNode bt0 bt1

type Path = [Int]

data Lens a = Lens
  { lensObject :: a,
    lensPath :: Path
  }
  deriving (Show, Eq)

-- Class of types a where (Lens a) makes sense, i.e. there associated set and
-- get "methods".
class (Eq a) => Lensable a where
  get :: Lens a -> Maybe a
  set :: Lens a -> a -> a

-- QUESTION 3
instance Lensable BT where
  get = undefined
  set = undefined

bigBT :: Int -> BT
bigBT n = bb [0 .. (2 ^ n - 1)]
  where
    bb [] = BTLeaf ""
    bb [k] = BTLeaf (show k)
    bb l = BTNode (bb (take (length l `div` 2) l)) (bb (drop (length l `div` 2) l))

assertLensableBTGet =
  get (Lens (bigBT 4) [0, 1, 1, 1]) == Just (BTLeaf "7")

assertLensableBTSet =
  case (bt, set (Lens bt [0, 1]) leaf) of
    (BTNode (BTNode x y) z, BTNode (BTNode x' y') z') ->
      y' == leaf && x == x' && z == z'
    _ -> False
  where
    bt = bigBT 4
    leaf = BTLeaf "17"

-- QUESTION 4
-- Does the treeish object have a subtree (possibly the whole tree) that
-- satisfies the predicate?
hasSubtree :: (Treeish a) => (a -> Bool) -> a -> Bool
hasSubtree = undefined

assertHasSubtree =
  hasSubtree (== bigBT 1) (bigBT 2)

-- QUESTION 5
-- The path to the subtree that satisfies the predicate, if any.
findSubtree :: (Treeish a) => (a -> Bool) -> a -> Maybe Path
findSubtree = undefined

assertFindSubtree =
  Just [0, 0] == findSubtree (== bigBT 1) (bigBT 3)

-- The "SK calculus" is a programming language that was mentioned early in the
-- course. You don't need to remember *anything* about it. The syntax is very
-- simple: there are "constants" S and K and all you can do is apply them. E.g.
-- S (K S) is K applied to (K S) which is K applied to S. We use the Haskell
-- convention for omitting parentheses, so e.g. S K S K is the same as
-- ((S K) S) K. This is just for readability in the comments -- the programs
-- below don't need to know this.
-- We also include "variables", styled as Atoms as in our Scheme
-- representation.
data SK = Atom String | K | S | App SK SK deriving (Show, Eq)

infixl 9 @

-- @ is just an infix version of App used to pretty-ify some code.
(@) :: SK -> SK -> SK
sk0 @ sk1 = App sk0 sk1

-- Like Assignment 9, but no option for "built-in" functions.
data Rule = Rule
  { ruleLhs :: SK,
    ruleRhs :: SK
  }
  deriving (Eq, Show)

-- Instead of Data.Map, we'll just use the usual association lists for
-- substitions.
type Subst = [(String, SK)]

-- Apply a substitution to an SK expression.
applySubst :: Subst -> SK -> SK
applySubst ((x, sk) : _) (Atom y) | x == y = sk
applySubst (_ : s) (Atom y) = applySubst s (Atom y)
applySubst s (App sk0 sk1) = App (applySubst s sk0) (applySubst s sk1)
applySubst _ sk = sk

-- The reduction rules for SK. There are two. In concrete syntax, and fully
-- parenthesizing, they are:
-- 1. (K st0) st1 -> st0
-- 2. ((S st0) st1) st2 -> (st0 st2) (st1 st2)
skRuleSet :: [Rule]
skRuleSet =
  let x = Atom "x"
      y = Atom "y"
      f = Atom "f"
      g = Atom "g"
   in [ Rule {ruleLhs = K @ x @ y, ruleRhs = x},
        Rule {ruleLhs = S @ f @ g @ x, ruleRhs = f @ x @ (g @ x)}
      ]

-- QUESTION 6
-- Match sk0 sk1: Compute a substition s, if one exists, such that
-- applySubst s sk0 = sk1. You will find useful the function combineMatches,
-- defined below.
match :: SK -> SK -> Maybe Subst
match = undefined

assertMatch =
  let x = Atom "x"
      y = Atom "y"
   in match (S @ x @ y @ K) (S @ K @ (S @ K) @ K)
        == Just [("x", K), ("y", App S K)]

combineMatches :: Maybe Subst -> Maybe Subst -> Maybe Subst
combineMatches ms0 ms1 = do
  s0 <- ms0
  s1 <- ms1
  failUnless (compatibleSubsts s0 s1)
  return (s0 ++ s1)

compatibleSubsts :: Subst -> Subst -> Bool
compatibleSubsts s0 s1 =
  all
    ( \(x, sk0) -> case lookup x s1 of
        Nothing -> True
        Just sk1 -> sk0 == sk1
    )
    s0

applyRule :: Rule -> SK -> Maybe SK
applyRule (Rule lhs rhs) sk = do
  s <- match lhs sk
  return $ applySubst s rhs

-- QUESTION 7
-- applyRuleSet sk: The result of applying the first rule in skRuleSet that
-- succeeds on sk.
applyRuleSet :: SK -> Maybe SK
applyRuleSet = undefined

assertApplyRuleSet0 =
  applyRuleSet (S @ K @ S @ K) == Just (App (App K K) (App S K))

assertApplyRuleSet1 =
  let u = Atom "u"
      v = Atom "v"
   in applyRuleSet (K @ (u @ K) @ v) == Just (App (Atom "u") K)

-- QUESTION 8
-- Do at least one reduction step, i.e. rule application, somewhere in the expression.
-- Nothing is returned exactly if there is no rule in skRuleSet and no subexpression
-- (including the whole expression) where the rule application succeeds.
reduce1 :: SK -> Maybe SK
reduce1 = undefined

-- Complete reduce an SK expression, i.e. apply reductions anywhere in the
-- expression until no longer possible.
reduce :: SK -> SK
reduce sk = case reduce1 sk of
  Nothing -> sk
  Just sk' -> reduce sk'

assertReduce =
  reduce (S @ K @ S @ K) == K

----------------
-- Utilities ---
----------------

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p l =
  findIt 0 l
  where
    findIt n (x : _) | p x = Just n
    findIt n (_ : l) = findIt (n + 1) l
    findIt _ _ = Nothing

firstSuccess :: [Maybe a] -> Maybe a
firstSuccess [] = Nothing
firstSuccess (Just x : _) = Just x
firstSuccess (_ : mxs) = firstSuccess mxs

failUnless :: Bool -> Maybe ()
failUnless b = if b then Just () else Nothing

succeeds :: Maybe a -> Bool
succeeds (Just x) = True
succeeds _ = False
