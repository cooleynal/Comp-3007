-- Implement a Map type and use it to count word occurrences in a document.

type Map a b = [(a, b)]

empty :: Map a b
empty = []

find :: (Eq a) => a -> Map a b -> Maybe b
find x [] = Nothing
find x ((y, v) : m) | x == y = Just v
find x ((y, v) : m) = find x m

update :: (Eq a) => a -> b -> Map a b -> Map a b
update x v [] = [(x, v)]
update x v ((x', v') : m) | x == x' = (x, v) : m
update x v ((x', v') : m) = (x', v') : update x v m

keys :: (Eq a) => Map a b -> [a]
keys m = unDup (map fst m)

unDup :: (Eq a) => [a] -> [a]
unDup [] = []
unDup (x : l) = x : filter (/= x) (unDup l)

isOptimal :: (Eq a) => Map a b -> Bool
isOptimal [] = True
isOptimal ((x, v) : m) =
  isOptimal m && all (/= x) (map fst m)

optimize :: (Eq a) => Map a b -> Map a b
optimize [] = []
optimize ((x, v) : m) =
  let p (y, _) = (y /= x)
   in (x, v) : filter p (optimize m)

optimize1 :: (Eq a) => Map a b -> Map a b
optimize1 [] = []
optimize1 ((x, v) : m) =
  (x, v) : filter (\pr -> fst pr /= x) (optimize1 m)

optimize2 :: (Eq a) => Map a b -> Map a b
optimize2 [] = []
optimize2 ((x, v) : m) =
  (x, v) : filter (\(y, _) -> y /= x) (optimize2 m)

optimize3 :: (Eq a) => Map a b -> Map a b
optimize3 [] = []
optimize3 ((x, v) : m) =
  (x, v) : filter ((/= x) . fst) (optimize3 m)

mapValues :: (b -> c) -> Map a b -> Map a c
mapValues f m =
  map (\(x, v) -> (x, f v)) m

updateValue :: (Eq a) => a -> (b -> b) -> Map a b -> Map a b
updateValue x f m =
  map adjustPair m
  where
    adjustPair (y, v)
      | y == x =
          (y, f v)
    adjustPair pr = pr

meg :: Map Int Int
meg = [(1, 2), (2, 3), (3, 1)]

data Doc = Doc [String]

freqMap :: Doc -> Map String Int
freqMap (Doc []) = empty
freqMap (Doc (word : rest)) =
  let restMap = freqMap (Doc rest)
   in if find word restMap == Nothing
        then (word, 1) : restMap
        else updateValue word (+ 1) restMap
