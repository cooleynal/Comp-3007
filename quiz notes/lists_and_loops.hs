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