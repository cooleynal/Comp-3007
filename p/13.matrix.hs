data DB = DB [[Int]]
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


sumColumns :: [[Int]] -> [Int]
sumColumns rows = foldr (zipWith (+)) (replicate (length (head rows)) 0) rows

-- make zeros
-- (replicate (length (head rows)) 0)
-- add
-- (zipWith (+)) (replicate (length (head rows)) 0) rows

sumByColumn :: DB -> [Int]
sumByColumn (DB rows) = sumColumns rows

main :: IO ()
main = print $ sumByColumn db


-- zipWith (+) (zipWith (+) [0, 0]) [1, 2]