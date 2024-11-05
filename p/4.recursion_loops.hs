
-- normal recursion
-- fact 5
fact :: Int -> Int
fact 0 = 1
fact a = a * fact (a - 1)

-- tail recursion
-- factTail 5
factTail :: Int -> Int
factTail a = helper a 1
  where
    helper 0 acc = acc
    helper a acc = helper (a-1) (a * acc)


-- normal recursion
-- fib 33
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- tail recursion
-- fibTail 33
fibTail :: Int -> Int
fibTail a = fibTail' a (0, 1)
  where
    fibTail' :: Int -> (Int, Int) -> Int
    fibTail' 0 (a, _) = a
    fibTail' 1 (_, b) = b
    fibTail' n (a, b) = fibTail' (n - 1) (b, a + b)


-- stirling 15 8
-- stirling 55 2
stirling :: Int -> Int -> Int
stirling n 1 = 1
stirling 0 0 = 1
stirling n 0 = 0
stirling n k
  | n < k     = 0
  | otherwise = stirling (n - 1) (k - 1) + k * stirling (n - 1) k




listize :: [a] -> [[a]]
listize [] = []
listize (x : l) = [x] : listize l

append :: [a] -> a -> [a]
append [] y = [y]
append (x : l) y = x : append l y


data MyList = Empty | Cons String MyList
  deriving (Show)

firstElement :: MyList -> String
firstElement Empty = ""
firstElement (Cons x _) = x


myEmptyList :: MyList
myEmptyList = Empty

myList :: MyList
myList = Cons "Hello" (Cons "World" Empty)

data DB = DB [[Int]] deriving (Show)

db :: DB
db =
  DB
    [ [1, 2075, 6, 1271, 1930]
    , [2, 254, 239, 65, 571]
    , [3, 1510, 301, 1570, 703]
    , [4, 271, 2009, 2085, 1899]
    , [5, 1974, 449, 1975, 580]
    , [6, 1329, 1970, 1152, 608]
    , [7, 2019, 724, 468, 953]
    , [8, 1404, 2155, 1671, 324]
    , [9, 1714, 1135, 738, 959]
    , [10, 1223, 1398, 1466, 876]
    ]


-- ghci> printDB db
printDB :: DB -> IO ()
printDB (DB rows) = printRows rows
  where
    printRows [] = return ()          -- Base case: empty list of rows
    printRows (row:rest) = do
      printRow row                    -- Print the current row
      putStrLn ""                     -- Print a newline after each row
      printRows rest                  -- Recur with the remaining rows

    printRow [] = return ()           -- Base case: empty row
    printRow (x:xs) = do
      putStr (show x ++ " ")          -- Print the current element followed by a space
      printRow xs


dd :: [Integer]
dd = [3, 1510, 301, 1570, 703]


-- sumList1 [3, 1510, 301, 1570, 703]

sumList1 :: [Int] -> Int
sumList1 = foldl (+) 0

-- runQuery (DB x) SumDB = show $ foldr (\x acc -> acc + sum (tail x)) 0 x

-- sumList2 [3, 1510, 301, 1570, 703]
sumList2 :: [Int] -> Int
sumList2 = foldr (+) 0


manualSum :: [Integer] -> Integer
manualSum [] = 0
manualSum (x:xs) = x + (manualSum xs)

-- runQuery (DB x) SumDB = show $ foldr (\x acc -> acc + sum (tail x)) 0 x

-- sumer :: DB -> Int
-- sumer [[]] = 0
-- sumer (f:l) = sum f + sumer l

-- sumer :: DB -> Int
-- sumer (DB rows) = sumRows rows

-- sumRows :: [[Int]] -> Int
-- sumRows [] = 0
-- sumRows (row:rows) = sum row + sumRows rows



f :: Integer -> Integer
f 0 = 1
f n = n * f (n - 1)

g :: Integer -> Float
g n = fromIntegral (n + 4)

results :: [Float]
results = [g n | n <- [1..100]]

r :: [Float]
r = [n | n <- [1..100]]




sumer :: DB -> Int
sumer (DB rows) =
    case rows of
        [] -> 0
        (row:rest) -> sum (tail row) + sumer (DB rest)




data Dict = Mt | Entry String String Dict
  deriving (Show)

eg :: Dict
eg = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
eg1 :: Dict
eg1 = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt))


-- firstKey eg = "Bingo"
firstKey :: Dict -> String -- firstKey is a function with Dict input and String output
firstKey Mt = ""
firstKey (Entry k v p) = k

-- firstValue eg = "Bongo"
firstValue :: Dict -> String
firstValue Mt = ""
firstValue (Entry k v p) = v

-- removeFirst eg = Entry "Baz" "Ola" (Entry "Big" "Deal" Mt)
removeFirst :: Dict -> Dict
removeFirst Mt = Mt
removeFirst (Entry k v p) = p


stringify :: Dict -> String
stringify Mt = ""
stringify (Entry k v Mt)  = k ++ ":" ++ v
stringify (Entry k v p)   = k ++ ":" ++ v ++ "," ++ stringify p

-- stringify :: Dict -> String
-- stringify Mt = ""
-- stringify (Entry key value point) = key ++ ":" ++ value ++ check point
--     where
--         check Mt = ""
--         check r = "," ++ (stringify r)



-- rev eg = Entry "Bongo" "Bingo" (Entry "Ola" "Baz" (Entry "Deal" "Big" Mt))
rev :: Dict -> Dict
rev Mt = Mt
rev (Entry k v p) = Entry v k (rev p)

-- reverse list
-- rev2 eg
rev2 :: Dict -> Dict
rev2 dict = rev2h dict Mt

rev2h :: Dict -> Dict -> Dict
rev2h Mt acc = acc
rev2h (Entry k v p) acc = rev2h p (Entry k v acc)

rev3 :: Dict -> Dict
rev3 dict = rev3h dict Mt
  where
    rev3h Mt acc = acc
    rev3h (Entry k v p) acc = rev3h p (Entry k v acc)

rev4 :: Dict -> Dict
rev4 dict =
  let
    rev4h Mt acc = acc
    rev4h (Entry k v p) acc = rev4h p (Entry k v acc)
  in
    rev4h dict Mt


-- find "Baz" eg = "Ola"
-- find "Egaah" eg = ""
find :: String -> Dict -> String -- find is a function with two inputs and String output
find s Mt = ""
find s (Entry k v p)
  | s == k = v
  | s == v = k
  | otherwise = find s p

-- find :: String -> Dict -> String
-- find _ Mt = ""
-- find target_key (Entry key value point)
--     | target_key == key = value
--     | otherwise = (find target_key point)


-- Replace all occurrences (as a key or a value) of badWord by "###"
-- censor "Ola" eg = Entry "Bingo" "Bongo" (Entry "Baz" "###"" (Entry "Big" "Deal" Mt))
-- censor "Baz" (censor "Ola" eg) = Entry "Bingo" "Bongo" (Entry "###" "###"" (Entry "Big" "Deal" Mt))
-- censor "Baz" eg

censor :: String -> Dict -> Dict
censor _ Mt = Mt
censor s (Entry k v p) = Entry (hider k) (hider v) (censor s p)
  where
    hider candidate =
      if candidate == s then "###"
      else candidate


-- censor :: String -> Dict -> Dict
-- censor x Mt = Mt
-- censor x (Entry k y d) =
--   Entry (if k == x then "###" else k) (if y == x then "###" else y) (censor x d)



censor1 :: String -> Dict -> Dict
censor1 s Mt = Mt
censor1 s (Entry k v p) = Entry (hider k) v (censor1 s p)
  where
    hider candidate
      | candidate == s = "###"
      | otherwise = candidate


-- remove "Bingo" eg
remove :: String -> Dict -> Dict
remove _ Mt = Mt
remove s (Entry k v p)
  | s == k || s == v  = remove s p
  | otherwise         = Entry k v (remove s p)


-- remove :: String -> Dict -> Dict
-- remove x Mt = Mt
-- remove x (Entry k y d) =
--   if x == k
--     then remove x d
--     else Entry k y (remove x d)


-- removeDoubles (Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt)))
-- = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" Mt)
removeDoubles :: Dict -> Dict
removeDoubles Mt = Mt
removeDoubles (Entry k v p) = Entry k v (remove k (removeDoubles p))


-- removeDoubles :: Dict -> Dict
-- removeDoubles = collector [] where
--     collector :: [String] -> Dict -> Dict
--     collector _ Mt = Mt
--     collector seenKeys (Entry key value point)
--         | key `elem` seenKeys = (collector seenKeys point)
--         | otherwise = Entry key value (collector (key : seenKeys) point)


-- Specification:
-- 1) for every key k and dictionaries d1 and d2,
--    find k (combine d1 d2) = find k d1  -- if (find k d1) is not ""
--    find k (combine d1 d2) = find k d2  -- otherwise
-- 2) removeDoubles (combine d1 d2) = combine d1 d2

-- combine eg eg1
-- combine :: Dict -> Dict -> Dict
-- combine Mt Mt = Mt
-- combine Mt m2 = combine m2 Mt
-- combine (Entry k1 v1 p1) m2 = Entry k1 v1 (combine p1 m2)

combine :: Dict -> Dict -> Dict
combine Mt Mt = Mt
combine Mt m2 = m2
combine m1 Mt = m1
combine (Entry k1 v1 p1) m2 = Entry k1 v1 (combine p1 m2)


-- combine :: Dict -> Dict -> Dict
-- combine Mt d2 = removeDoubles d2
-- combine (Entry x y d1) d2 = Entry x y (remove x (combine d1 d2))


findr :: String -> Dict -> Bool
findr _ Mt = False
findr s (Entry k _ p)
    | s == k    = True
    | otherwise = findr s p





combiner :: Dict -> Dict -> Dict
combiner m1 m2 = removeDoubles ( combine m1 m2)







main :: IO ()
main = do
  -- Testing factorial functions
  putStrLn $ "Factorial of 5: " ++ show (fact 5)
  putStrLn ""
  putStrLn $ "Tail-recursive factorial of 5: " ++ show (factTail 5)
  putStrLn ""

  -- Testing Fibonacci functions
  putStrLn $ "Fibonacci of 5: " ++ show (fib 5)
  putStrLn ""
  putStrLn $ "Tail-recursive Fibonacci of 5: " ++ show (fibTail 5)
  putStrLn ""

  -- Testing Stirling numbers
  putStrLn $ "Stirling number of n=7, k=3: " ++ show (stirling 7 3)
  putStrLn ""
  putStrLn $ "Stirling number of n=15, k=8: " ++ show (stirling 15 8)
  putStrLn ""
  putStrLn $ "Stirling number of n=55, k=2: " ++ show (stirling 55 2)
  putStrLn ""

  -- Testing listize and append
  let result = listize "hello"
  print result
  putStrLn ""
  let r2 = append [1, 2, 3, 4] 5
  print r2
  putStrLn ""

  -- Testing custom list (MyList) and firstElement
  putStrLn $ "First element of myList: " ++ firstElement myList
  putStrLn ""
  putStrLn $ "First element of myEmptyList: " ++ firstElement myEmptyList
  putStrLn ""

  -- Testing DB functions
  putStrLn "DB contents:"
  printDB db
  putStrLn ""

  putStrLn "Sum of all elements except the first in each row of the DB:"
  putStrLn ""
  putStrLn $ "Manual sum of dd: " ++ show (manualSum dd)
  putStrLn ""
  putStrLn $ "Sum using sumList1: " ++ show (sumList1 (map fromIntegral dd))
  putStrLn ""
  putStrLn $ "Sum using sumList2: " ++ show (sumList2 (map fromIntegral dd))
  putStrLn ""
  putStrLn $ "Sum of all columns (excluding the first one) in the DB: " ++ show (sumer db)
  putStrLn ""

  -- Printing results of function `g` applied to numbers from 1 to 100
  putStrLn "Results of function g for numbers 1 to 100:"
  print results
  putStrLn ""

  -- Checking if all elements in one list are present in another list
  putStrLn "Checking if all elements of [1, 2, 3] are in [1, 2, 3, 4]:"
  print $ all (`elem` [1, 2, 3, 4]) [1, 2, 3]
  putStrLn ""

  putStrLn (firstKey eg)
  putStrLn (firstValue eg)
  putStrLn (stringify (removeFirst eg))
  putStrLn (stringify eg)
  putStrLn (stringify (rev eg))
  putStrLn (stringify (rev2 eg))
  putStrLn (stringify (rev3 eg))
  putStrLn (stringify (rev4 eg))
  putStrLn (find "Baz" eg)
  putStrLn (find "Ola" eg)
  putStrLn (stringify (censor1 "Baz" eg))
  putStrLn (stringify (remove "Baz" eg))
