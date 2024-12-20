

data DB = DB [[Int]] deriving (Show)

data Query = GetRow Int | CountRows | Validate | SumColumns
  deriving (Show)

data Transformer = DeleteRow Int | AddRow [Int] | Sort
  deriving (Show)



dbUnsorted :: DB
dbUnsorted =
  DB
    [ [10, 1223, 1398, 1466, 876]
    , [9, 1714, 1135, 738, 959]
    , [8, 1404, 2155, 1671, 324]
    , [7, 2019, 724, 468, 953]
    , [5, 1974, 449, 1975, 580]
    , [6, 1329, 1970, 1152, 608]
    , [4, 271, 2009, 2085, 1899]
    , [3, 1510, 301, 1570, 703]
    , [2, 254, 239, 65, 571]
    , [1, 2075, 6, 1271, 1930]
    ]


sorted :: [Int] -> [[Int]] -> [[Int]]
sorted row [] = [row]
sorted row (r:rs)
    | head row <= head r = row : r : rs
    | otherwise = r : sorted row rs

insertionSort :: [[Int]] -> [[Int]]
insertionSort [] = []
insertionSort (x:xs) = sorted x (insertionSort xs)


-- sorter' :: [Int] -> [Int] -> [Int]
-- sorter' [] l = l
-- sorter' l _ =


-- sorter :: [Int] -> [Int]
-- sorter [] = []
-- sorter (x : xs) = sorter' l []
--   where
--     sorter' :: [Int] -> [Int] -> [Int]
--     sorter' [] acc = acc
--     sorter' (x:xs) acc = sorter' xs (insert x acc)


-- Curried function to add two numbers
add :: Int -> Int -> Int
add x y = x + y

-- Uncurried function to add two numbers
addUncurried :: (Int, Int) -> Int
addUncurried (x, y) = x + y




sorter :: [Int] -> [Int]
sorter [] = []
sorter (x:xs) = sorter' xs [x]
  where
    sorter' :: [Int] -> [Int] -> [Int]
    sorter' [] acc = acc
    sorter' (y:ys) acc = sorter' ys (insert y acc)
      where
        insert :: Int -> [Int] -> [Int]
        insert y [] = [y]
        insert y (z:zs)
          | y <= z    = y : z : zs
          | otherwise = z : insert y zs

-- Quicksort implementation
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted



(+++) :: Int -> Int -> Int
x +++ y = x + 2 * y

main :: IO ()
main = do
  let (DB unsortedRows) = dbUnsorted
  let sortedRows = insertionSort unsortedRows
  putStrLn $ "Sorted Database: " ++ show sortedRows
  putStrLn ""
  let rdm = [12, 67, 45, 23, 88, 1, 99, 76, 34, 50, 1 , 5, 8, 90]
  let s = sorter rdm
  putStrLn $ show rdm ++ "Sort: " ++ show s


  -- Currying example
  let addFive = add 5
  print (addFive 10)               -- Outputs: 15

  -- Uncurrying example
  let tuple = (5, 10)
  print (addUncurried tuple)        -- Outputs: 15

  let a = 5
      b = 3
      result = a +++ b
  putStrLn $ "Using the operator (+++): " ++ show a ++ " +++ " ++ show b ++ " = " ++ show result
