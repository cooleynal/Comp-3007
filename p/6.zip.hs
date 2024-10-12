
data DB = DB [[Int]]
  deriving (Show)

data Query = GetRow Int | CountRows | Validate | SumColumns
  deriving (Show)

data Transformer = DeleteRow Int | AddRow [Int] | Sort
  deriving (Show)


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



db1 :: DB
db1 =
  DB
    [ [1, 2075, 6, 1271, 12]
    , [2, 254, 239, 65, 571]
    , [3, 1510, 301, 1570, 703]
    , [4, 271, 2009, 2085, 1899]
    , [5, 1974, 449, 1975, 580]
    , [6, 1329, 13, 1152, 608]
    , [7, 2019, 724, 468, 953]
    , [8, 1404, 2155, 1671, 324]
    , [9, 1714, 1135, 738, 959]
    , [10, 1223, 1398, 1466, 876]
    ]


l1 :: [Integer]
l1 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
l2 :: [Integer]
l2 = [1, 1, 1, 1, 1, 1, 1, 1, 3]

sumColumns :: DB -> [Int]
sumColumns (DB rows) =
  let
    numCols = length (head rows)
    zeros = replicate numCols 0
  in
    foldr (zipWith (+)) zeros rows

sumColumns1 :: DB -> [Int]
sumColumns1 (DB rows) = foldr (zipWith (+)) zeros rows
    where
    numCols = length (head rows)
    zeros = replicate numCols 0


-- any :: Foldable t => (a -> Bool) -> t a -> Bool

equivDB:: DB-> DB -> Bool
equivDB (DB db1) (DB db2) =
  foldr (\(row1, row2) acc -> (row1 == row2) && acc) True (zip db1 db2)


main :: IO ()
main = do
    putStrLn "Hello, World!"
    let result = zipWith (-) l1 l2
    let cs = sumColumns db
    let cs1 = sumColumns1 db
    let eq = equivDB db db1

    putStrLn $ "The result of adding l1 and l2 is: " ++ show result
    putStrLn $ "The sum of each column is: " ++ show cs
    putStrLn $ "The sum of each column is: " ++ show cs1
    putStrLn $ "Tdbzzis: " ++ show eq
