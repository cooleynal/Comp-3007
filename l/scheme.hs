{- MultiParamTypeClasses -}
{- FlexibleInstances -}

import Data.Map qualified as M
import Data.Maybe
import Data.Text (pack, unpack)
import LispVal qualified as L
import Parser
-- import Text.Pretty.Simple -- do i need this?

data Exp
  = Atom String
  | List [Exp]
  | Number Integer
  | String String
  | Nil
  | Bool Bool
  deriving (Eq, Ord, Show)

type Path = [Int]

data Lens = Lens
  { lensExp :: Exp,
    lensPath :: Path
  }
  deriving (Show, Eq)

class Tr a b where
  tr :: a -> b

get :: Lens -> Exp
get (Lens e p) = getUsingPath e p

getUsingPath :: Exp -> Path -> Exp
getUsingPath e [] = e
getUsingPath (List es) (k : ks)
  | 0 <= k && k < length es =
      getUsingPath (es !! k) ks
getUsingPath e p = error $ "getUsingPath: " ++ show p ++ show e

set :: Lens -> Exp -> Exp
set (Lens e p) e0 = setUsingPath p e e0

setUsingPath :: Path -> Exp -> Exp -> Exp
setUsingPath [] e e0 = e0
setUsingPath (k : ks) (List es) e0
  | 0 <= k && k < length es =
      List $ replaceNth k (setUsingPath ks (es !! k) e0) es
setUsingPath p e e0 = error $ "getUsingPath: " ++ show p ++ show e

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x xs
  | 0 <= n && n < length xs =
      take n xs ++ [x] ++ drop (n + 1) xs

find :: Exp -> (Exp -> Bool) -> Maybe Lens
find e p = fmap (Lens e . reverse) $ findPath e p []

-- returns path in reverse order
findPath :: Exp -> (Exp -> Bool) -> Path -> Maybe Path
findPath e p ks | p e = Just ks
findPath (List es) p ks =
  case mapMaybe (\(k, e) -> findPath e p (k : ks)) (zip [0 .. length es - 1] es) of
    [] -> Nothing
    (ks : _) -> Just ks
findPath _ _ _ = Nothing

unparse :: Exp -> String
unparse (Atom str) = str
unparse (List es) = "(" ++ unwords (map unparse es) ++ ")"
unparse (Number n) = show n
unparse (String str) = show str
unparse Nil = "'()"
unparse (Bool True) = "#t"
unparse (Bool False) = "#f"

instance Tr L.LispVal Exp where
  tr (L.Atom txt) = Atom (unpack txt)
  tr (L.List es) = List $ map tr es
  tr (L.Number n) = Number n
  tr (L.String txt) = String (unpack txt)
  tr L.Nil = Nil
  tr (L.Bool b) = Bool b

parseExp :: String -> Exp
parseExp str = case readExpr (pack str) of
  Right v -> tr v
  Left e -> error $ show e

factStr :: String
factStr =
  unlines
    [ "(define (factorial n) ",
      "(if (eq? n 0) ",
      "1 ",
      "(* n (factorial (- n 1)))))"
    ]

fact :: Exp
fact = parseExp factStr

lens0 = fromJust $ find fact (== Atom "*")
