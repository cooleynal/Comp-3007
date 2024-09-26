-- import Data.List (sort)

-- Assignment 3 due  Sunday Sept 22 23:59
--
-- This assignment is intended to build some proficiency with
--
-- - pattern matching with guards and recursion - map/filter/all
--
-- Before starting, make sure you understand the above as covered in the
-- lectures and Learn You a Haskell. If you don't, you might waste a lot of time
-- because you aren't aware of all the needed techiques.
--
-- Some tips.
--
-- - If you want to learn what a function does, the best way sometimes is to use
-- ":type" in ghci. Then try the function on couple of inputs. The type you
-- found will tell you how to build the inputs.
--
-- - Don't import anything. The autograder will reject any file with an import
-- statement.
--
-- - You will need "helper" functiohs, i.e. functions that help you build the
-- required functions.
--
-- - Look at the Haskell "Prelude" documentation. The Prelude is a library
-- that's automatically loaded when ghci (or the haskell compiler) is started.
-- You are welcome to use anything there. This applies to future assignments and
-- tests. However, for this assignment, you will only need (at most): map,
-- filter, all, show, elem, head, tail, and basic boolean stuff.
--
-- You might find "sections" useful (see Sept 16 lecture)\
--
-- Submission: submit this file with the "undefined" functions replaced by
-- working code. You can add whatever new code/types you find useful, as long as you
-- don't change any of the provided types or the types/names of the provided
-- functions.

---------------------------------------------------------------------------------

-- DB is a type of simple "databases". A database (here) is a list of "rows",
-- where a row is a list of integers. The first element of a row, if it exists,
-- is called its "key".
--
-- A database db is "valid" if all rows are non-empty and have the same length,
-- and no two rows have the same key. Each row can be thought of a key followed
-- by a list of "column values". The example at the end of the file has four
-- columns (since we don't count the keys as a column). In the row [4, 271,
-- 2009, 2085, 1899], 4 is the key and 271, 2009, 2085, 1899 are the values for
-- the first, second, third and fourth columns.
data DB = DB [[Int]] deriving (Show)

data Query = GetRow Int | CountRows | Validate | SumColumns
  deriving (Show)

data Transformer = DeleteRow Int | AddRow [Int] | Sort
  deriving (Show)

-- [Utility supplied for convenience in testing.]
-- Print out a nice display of a database in ghci. You don't need to know how
-- this works, but if you're interested, the result type IO() is the type of IO
-- "actions". The ghci interpreter knows how to run these actions.
printDB :: DB -> IO ()
printDB (DB []) = putStrLn "Empty"
printDB (DB nss) =
  let maxSize = maximum (map (maximum . map (length . show)) nss) -- assume db valid
      pad str = " " ++ replicate (maxSize - length str) ' ' ++ str
      prepRow = concatMap (pad . show)
   in mapM_ (putStrLn . prepRow) nss


-- Two databases are "equivalent"  if every row in either of them is also in the
-- other.
-- equivDB db dbUnsorted
equivDB :: DB -> DB -> Bool
equivDB (DB rows1) (DB rows2) =
  all (`elem` rows1) rows2 && all (`elem` rows2) rows1


-- runQuery db q returns a string representing the result of running the query q
-- on the database db. It is assumed that the input database is valid.
--
-- Query results:
--   GetRow n: the row with key n.
--   CountRows: the number of rows
--   SumColumns: a list of the sums of the columns (note: keys not included)
--   Validate: True if db is valid, False otherwise
-- In each of the above cases, use the function "show" to get the string
-- representation of the value.


runQuery :: DB -> Query -> String
-- runQuery db (GetRow 2)
runQuery (DB rows) (GetRow key) = show (rows !! key)

-- runQuery db CountRows
runQuery (DB rows) CountRows = show (length rows)

-- runQuery db SumColumns
-- runQuery (DB rows) SumColumns =
--   let
--     start = 0
--     size = length rows !! start -- first row (or length (head rows))
--     indices = [start .. size - 1]
--   in
--     map flip (!!) indices
-- -- mf = flip (!!) 1
-- -- sum (  map mf [[1, 2, 3], [4, 5, 6]])




db1 :: [[Int]]
db1 =
  [[1, 2, 3],
    [4, 5, 6]]

-- myGeneratorFunc:: Int -> ([[Int]] -> Int)
mf :: Int -> [[Int]] ->Int
-- mf :: Num a => Int -> [[a]] -> a
mf x xs = sum $ map (flip (!!) x) xs
-- mf 1 db1

-- mg x = sum . map (flip (!!) x)
-- so = mg 1

-- so db

-- myGeneratorFunc x = sum . map (flip (!!) x)
-- sumColumns = map myGeneratorFunc [1 .. length db]
-- map ($ db) sumColumns




-- runTransform db t: "transform" the database db using the transformoer
-- operation t.
--
-- DeleteRow n: delete the row with key n
--
-- AddRow ns: add the row ns to the database if doing so keeps the database
-- valid, otherwise return the original database.
--
-- Sort: reorder the rows in the database so that their keys are non-decreasing
-- of keys.
runTransformer :: DB -> Transformer -> DB
runTransformer = undefined

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
