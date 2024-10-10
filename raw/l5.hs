-- Implement a Map type and use it to count word occurrences in a document.

-- This type is viewed as identical to [(a,b)] in Haskell
-- Better: data Map a b = Map [(a,b)]
-- [Haskell prefers: newtype Map a b = Map [(a,b)]]
type Map a b = [(a, b)] -- (a,b) type of pairs e.g. (1,2) :: (Int,Int)

empty :: Map a b
empty = []

find :: Eq a => a -> Map a b -> Maybe b -- i.e. find :: a -> Map a b -> Maybe b for every type a 
                                        -- where == is defined
find x [] = Nothing
find x ((y, v) : m) | x == y = Just v
find x (_ : m) = find x m

update :: Eq a => a -> b -> Map a b -> Map a b
update x v [] = [(x,v)]
update x v ( (x',v') : m) | x == x' = (x,v) : m
update x v ( (x',v') : m)  = (x',v') : update x v m

-- eg keys meg = [1,2,3]
keys :: Eq a => Map a b -> [a]
keys m = noDups (map fst m)

noDups :: Eq a => [a] -> [a]
noDups [] = []
-- noDups (x : l) = x : filter (/= x) (noDups l)
-- noDups (x : l) = x : filter (\x' -> x' /= x) (noDups l)
-- noDups (x : l) = 
--   let f x' = x' /= x
--   in
--   x : filter f (noDups l)
noDups (x : l) = 
  x : filter f (noDups l)
  where f x' = x' /= x


meg :: [(Int, Int)]
meg = [(1, 2), (2, 3), (3, 1)] -- "function" or "map" 1 ↦ 2, 2 ↦ 3 and 3 ↦ 1
meg' = (2,17) : meg
