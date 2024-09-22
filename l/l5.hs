-- Implement a Map type and use it to count word occurrences in a document.

type Map a b = [(a, b)]





empty :: Map a b
empty = []

myMap :: Map Int String
myMap =
  [ (1, "name")
  , (30, "age")
  , (170, "height")
  , (65, "weight")
  , (65, "weight2")
  , (100, "score")
  ]


--   find 1 myMap
find :: (Eq a, Eq b) => a -> Map a b -> Maybe b
find x [] = Nothing
find x ((y, v) : m) | x == y = Just v
find x ((y, v) : m) = find x m


-- update 5 "rawr" myMap
-- update 1 "rawr" myMap
update :: (Eq a) => a -> b -> Map a b -> Map a b
update _ _ [] = []
update x v ((x', v') : m) | x == x' = (x, v) : m
update x v ((x', v') : m) = (x', v') : update x v m

-- keys myMap
keys :: (Eq a) => Map a b -> [a]
keys m = unDup (map fst m)



unDup :: (Eq a) => [a] -> [a]
unDup [] = []
unDup (x : l) = x : filter (/= x) (unDup l)

-- filter (/= 1) [1, 2, 2, 3, 1, 4, 3]
-- [1,3,1,4,3]



-- isOptimal myMap
-- isOptimal (unDup myMap)
isOptimal :: (Eq a) => Map a b -> Bool
isOptimal [] = True
isOptimal ((x, v) : m) =
  isOptimal m && all (/= x) (map fst m)


--   isOptimal (optimize myMap)
-- optimize myMap
optimize :: (Eq a) => Map a b -> Map a b
optimize [] = []
optimize ((x, v) : m) =
  let p (y, _) = (y /= x)
   in (x, v) : filter p (optimize m)


--    squareList [1, 2, 3, 4]
--    [1,4,9,16]
squareList :: [Int] -> [Int]
-- squareList nums = map (\x -> x * x) nums
squareList = map (^2)


-- A lambda function in Haskell is defined using the syntax \parameter -> expression. The backslash (\) indicates that it's an anonymous function.
-- In your case, \pr -> fst pr /= x is a lambda function that takes one argument (pr), which is expected to be a tuple.


optimizez :: (Eq a) => Map a b -> Map a b
optimizez [] = []
optimizez ((x, v) : m) =
  (x, v) : filter notEqualX (optimizez m)
  where
    notEqualX (key, _) = key /= x


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


-- mapValues (*2) [("a", 1), ("b", 2), ("c",3) ]
-- [("a",2),("b",4),("c",6)]


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



-- ghci> let document = Doc ["hello", "world", "hello", "haskell"]
-- ghci> freqMap document
-- [("world",1),("hello",2),("haskell",1)]

freqMap :: Doc -> Map String Int
freqMap (Doc []) = empty
freqMap (Doc (word : rest)) =
  let restMap = freqMap (Doc rest)
   in if find word restMap == Nothing
        then (word, 1) : restMap
        else updateValue word (+ 1) restMap


-- let document = Doc ["hello", "world", "hello", "haskell"]

-- frequency = freqMap document

-- eq

-- == (equal to)
-- /= (not equal to)


-- ord
-- < (less than)
-- <= (less than or equal to)
-- > (greater than)
-- >= (greater than or equal to)
-- compare (returns LT, EQ, or GT)


-- num
-- + (addition)
-- - (subtraction)
-- * (multiplication)
-- abs (absolute value)
-- signum (sign function)
-- fromInteger (convert from an integer)