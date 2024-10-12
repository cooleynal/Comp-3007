
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



stirling :: Int -> Int -> Int
stirling n 1 = 1
stirling 0 0 = 1
stirling n 0 = 0
stirling n k
  | n < k     = 0
  | otherwise = stirling (n - 1) (k - 1) + k * stirling (n - 1) k

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





listize :: [a] -> [[a]]
listize [] = []
listize (x : l) = [x] : listize l

append :: [a] -> a -> [a]
append [] y = [y]
append (x : l) y = x : append l y



main :: IO ()
main = do
  let result = listize "hello"
  let r2 = append [1, 2, 3, 4] 5
  print result
  print r2

  putStrLn $ "Factof 5: " ++ show (fact 5)
  putStrLn $ "FactTail of 5: " ++ show (factTail 5)

  putStrLn $ "Fibonacci of 5: " ++ show (fib 5)
  putStrLn $ "Fibonacci of 5: " ++ show (fibTail 5)

  putStrLn $ "stirling of n=7 k=3: " ++ show (stirling 7 3)





----------------------------------------------------

data MyList = Empty | Cons String MyList
  deriving (Show)

firstElement :: MyList -> String
firstElement Empty = ""  -- Return an empty string if the list is empty
firstElement (Cons x _) = x  -- Return the first element if the list is non-empty

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


sumer :: DB -> Int
sumer (DB rows) =
    case rows of
        [] -> 0
        (row:rest) -> sum (tail row) + sumer (DB rest)


----------------------------------------------------
