-- Define the database type
data DB = DBzz [[Int]] | Der Int deriving (Show)

-- Example database instance
db :: DB
db =
  DBzz
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

-- Define another example database instance for comparison
dbUnsorted :: DB
dbUnsorted =
  DBzz
    [ [2, 254, 239, 65, 571]
    , [1, 2075, 6, 1271, 1930]
    , [3, 1510, 301, 1570, 703]
    , [4, 271, 2009, 2085, 1899]
    , [5, 1974, 449, 1975, 580]
    , [6, 1329, 1970, 1152, 608]
    , [7, 2019, 724, 468, 953]
    , [8, 1404, 2155, 1671, 324]
    , [9, 1714, 1135, 738, 959]
    , [10, 1223, 1398, 1466, 876]
    ]

-- Another example with a different structure
derpyInt :: DB
derpyInt = Der 5

-- Define a query type for database operations
data Query = GetRow Int | CountRows | Validate | SumColumns deriving (Show)

-- Define a transformer type for modifying the database
data Transformer = DeleteRow Int | AddRow [Int] | Sort deriving (Show)

-- Function to sum the columns of a database across 2 datatypes
sumColumns :: DB -> [Int]
sumColumns (Der _) = [0, 0, 0]
sumColumns (DBzz rows) = foldr (zipWith (+)) (replicate (length (head rows)) 0) (tail rows)

-- Function to run queries on the database
runQuery :: DB -> Query -> String
runQuery (DBzz db) (GetRow n) = show $ getRow db n
runQuery (DBzz db) CountRows = show $ countRows db
runQuery (DBzz db) SumColumns = show $ sumColumns (DBzz db)
runQuery (DBzz db) Validate = show $ isValid db

-- Function to get a specific row from the database
getRow :: [[Int]] -> Int -> [Int]
getRow db n = if n >= 0 && n < length db then db !! n else []

-- Function to count the rows in the database
countRows :: [[Int]] -> Int
countRows = length

-- Function to check if the database is valid
isValid :: [[Int]] -> Bool
isValid [] = True
isValid (x:xs) = allSameLength x xs && allDifferent (map head (x:xs))
  where
    allSameLength row = all ((== length row) . length)
    allDifferent lst = length lst == length (distinct lst)

-- Helper function to get distinct elements
distinct :: (Eq a) => [a] -> [a]
distinct [] = []
distinct (x:xs) = x : distinct (filter (/= x) xs)

-- Function to run transformations on the database
runTransformer :: DB -> Transformer -> DB
runTransformer (DBzz db) (DeleteRow n) = DBzz (deleteRow db n)
runTransformer (DBzz db) (AddRow newRow) = DBzz (addRow db newRow)
runTransformer (DBzz db) Sort = DBzz (sort db)

-- Function to delete a row from the database
deleteRow :: [[Int]] -> Int -> [[Int]]
deleteRow db n = filter ((/= n) . head) db

-- deleteRow :: [[Int]] -> Int -> [[Int]]
-- deleteRow [] n = []
-- deleteRow (x : xs) n
--   | n == head x = xs
--   | otherwise   = x : deleteRow xs n

-- Function to add a row to the database
addRow :: [[Int]] -> [Int] -> [[Int]]
addRow db newRow = if isValid (db ++ [newRow]) then sort (db ++ [newRow]) else db

-- addRow :: [[Int]] -> [Int] -> [[Int]]
-- addRow [] _ = []
-- addRow db [_] = db
-- addRow db a = sort $ db ++ [a]


-- Sorting function for the database
sort :: [[Int]] -> [[Int]]
sort [] = []
sort (x:xs) = insert (sort xs) x

-- Insertion function for sorted order
insert :: [[Int]] -> [Int] -> [[Int]]
insert [] ns = [ns]
insert (x:xs) ns
  | head ns <= head x = ns : x : xs
  | otherwise = x : insert xs ns


-- sort :: [[Int]] -> [[Int]]
-- sort [] = []
-- sort (ns : nss) =
--   insert (sort nss) ns

-- insert :: [[Int]] -> [Int] -> [[Int]]
-- insert [] ns = [ns]
-- insert (ns0 : nss) ns
--   | head ns <= head ns0 = ns : ns0 : nss
--   | otherwise = ns0 : insert nss ns

-- Function to check if all elements in one row exist in another
alEleRows :: [Int] -> [Int] -> Bool
alEleRows row1 row2 = length row1 == length row2 && all (`elem` row2) row1

-- Function to check if all values in rows are greater than a threshold
agt :: Int -> DB -> Bool
agt th (DBzz rows) = all (all (> th)) rows

-- Function to check if two databases are equivalent
equivDB :: DB -> DB -> Bool
equivDB (DBzz lss1) (DBzz lss2) =
  length lss1 == length lss2 &&
  all (`elem` lss2) lss1 &&
  all (`elem` lss1) lss2

-- Function to check if all elements in a list are different
allDifferent :: (Eq a) => [a] -> Bool
allDifferent xs = length xs == length (distinct xs)

-- Main function to run the program
main :: IO ()
main = do
  let r1 = equivDB db dbUnsorted
  putStrLn $ "Are the databases equivalent? " ++ show r1

  let r2 = runQuery db CountRows
  putStrLn $ "runQuery db CountRows? " ++ show r2

  let r3 = agt (-3) db
  putStrLn $ "agt -3 db? " ++ show r3

  let r4 = runQuery db SumColumns
  putStrLn $ "runQuery db SumColumns? " ++ show r4

  let r5 = runQuery db Validate
  putStrLn $ "runQuery db Validate " ++ show r5

  let r6 = allDifferent [1, 2, 3, 4, 5, 6, 11, 2]
  putStrLn $ "allDifferent [1, 2, 3, 4, 5, 6, 11, 2] " ++ show r6

  let r7 = runTransformer db (DeleteRow 3)
  putStrLn $ "runTransformer db (DeleteRow 3) " ++ show r7

  let r8 = runTransformer db (AddRow [22, 2, 3, 4, 5])
  putStrLn $ "runTransformer db (AddRow [22, 2, 3, 4, 5]) " ++ show r8
