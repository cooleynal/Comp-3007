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
data DB = DB [[Int]]
  deriving (Show)

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
printDB (DB db) =
  let maxSize = maximum (map (maximum . map (length . show)) db) -- assume db valid
      pad str = " " ++ replicate (maxSize - length str) ' ' ++ str
      prepRow = concatMap (pad . show)
   in mapM_ (putStrLn . prepRow) db

-- Two databases are "equivalent"  if every row in either of them is also in the
-- other.


-- all (`elem` [1, 2]) [1, 2, 3]
-- elem 2 [1, 2, 3]

-- does 1 2 3 exist in 1 2?
-- all (\x -> elem x [1, 2]) [1, 2, 3]

-- equivDB :: DB -> DB -> Bool
-- equivDB (DB lss1) (DB lss2) =
--   length lss1 == length lss2
--     && all (`elem` lss2) lss1
--     && all (`elem` lss1) lss2


-- printDB db
-- equivDB db dbUnsorted
-- equivDB :: DB -> DB -> Bool
-- equivDB (DB db1) (DB db2) = length db1 == length db2 && all in1 db1 && all in2 db2
--   where
--     in1 row = all (\x -> any (\subRow -> elem x subRow) db2) row
--     in2 row = all (\x -> any (\subRow -> elem x subRow) db1) row

equivDB :: DB -> DB -> Bool
equivDB (DB lss1) (DB lss2) =
  length lss1 == length lss2
    && all (`elem` lss2) lss1 && all (`elem` lss1) lss2

-- alEleRows :: [Int] -> [Int] -> Bool
-- alEleRows row1 row2 = length row1 == length row2 && all (`elem` row2) row1

alEleRows :: [Int] -> [Int] -> Bool
alEleRows row1 row2 =
    let
        sameLength = length row1 == length row2
        allInRow2 = all (`elem` row2) row1
    in
        sameLength && allInRow2



agt :: Int -> DB -> Bool
agt th (DB rows) = all gr rows
  where
    gr row = all (> th) row


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
runQuery (DB db) (GetRow n) = show $ getRow db n
runQuery (DB db) CountRows = show $ countRows db
runQuery (DB db) SumColumns = show $ sumColumns db
runQuery (DB db) Validate = show $ isValid db


-- runQuery db (GetRow 1)
getRow :: [[Int]] -> Int -> [Int]
getRow [[]] _ = []
getRow m i =
  let len = length m
  in
    if len >= 0 && i <= len
      then  m !! i
    else []

-- runQuery db CountRows
countRows :: [[Int]] -> Int
countRows [] = 0
countRows (x : xs) = 1 + countRows xs

-- countRows :: [[Int]] -> Int
-- countRows [] = 0
-- countRows x = length x

-- A database db is "valid" if all rows are non-empty and have the same length,
-- and no two rows have the same key. Each row can be thought of a key followed



-- isValid :: DB -> Bool
-- isValid (x :xs) = all eqlen xs
--   where eqlen row == length row



isValid :: (Eq a) => [[a]] -> Bool
isValid [] = True
isValid (x:xs) = all eqlen xs && allDifferent (map head (x:xs))
  where
    eqlen row = length row == length x
    allDifferent [] = True
    allDifferent (y:ys) = notElem y ys && allDifferent ys


allDifferent :: (Eq a) => [a] -> Bool
allDifferent xs = not $ any duplicate xs
  where
    duplicate y = any (== y) (tail xs)




transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss) = (x : map head xss) : transpose (xs : map tail xss)


-- runQuery db SumColumns
-- sumColumns :: [[Int]] -> [Int]
-- sumColumns [] = []
-- sumColumns db = map sum (transpose db)

-- myFunc = (\x y -> x+y)
-- myfunc = flip (!!) 1
-- map myFunc [[1,2,3],[4,5,6]]

sumColumns :: [[Int]] -> [Int]
sumColumns [] = []
sumColumns db = foldr sumer (replicate n 0) (map tail db)
  where
    n = length (head db) - 1 -- from right
    sumer row acc = zipWith (+) row acc





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
runTransformer (DB db) (DeleteRow n) = DB (deleteRow db n)
runTransformer (DB db) (AddRow newRow) = DB (addRow db newRow)
runTransformer (DB db) Sort = DB $ sort db



deleteRow :: [[Int]] -> Int -> [[Int]]
deleteRow [] n = []
deleteRow (x : xs) n
  | n == head x = xs
  | otherwise   = x : deleteRow xs n


addRow :: [[Int]] -> [Int] -> [[Int]]
addRow [] _ = []
addRow db [_] = db
addRow db a = sort $ db ++ [a]



sort :: [[Int]] -> [[Int]]
sort [] = []
sort (ns : nss) =
  insert (sort nss) ns

insert :: [[Int]] -> [Int] -> [[Int]]
insert [] ns = [ns]
insert (ns0 : nss) ns
  | head ns <= head ns0 = ns : ns0 : nss
  | otherwise = ns0 : insert nss ns




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



myFunc :: Num a => a -> a -> a
myFunc x y = x + 3 * y



main :: IO ()
main = do
  let r1 = equivDB db dbUnsorted
  putStrLn $ "Are the databases equivalent? " ++ show r1

  let r2 = runQuery db CountRows
  putStrLn $ "runQuery db CountRows? " ++ show r2

  let r3 = agt (-3) db
  putStrLn $ "agt 2 db? " ++ show r3

  let r4 = runQuery db SumColumns
  putStrLn $ "runQuery db SumColumns? " ++ show r4

  let r5 = runQuery db Validate
  putStrLn $ "runQuery db Validate " ++ show r5

  let r6 = allDifferent [1, 2, 3, 4, 5, 6, 11, 2]
  putStrLn $ "allDifferent [1, 2, 3, 4, 5, 6, 11, 2] " ++ show r6

  let r7 = runTransformer db (DeleteRow 3)
  putStrLn $ "runTransformer db (DeleteRow 3) " ++ show r7

  let r8 = runTransformer db (AddRow [-12, 2, 3, 4, 5, 6, 11, 2])
  putStrLn $ "runTransformer db (AddRow [22, 2, 3, 4, 5, 6, 11, 2]) " ++ show r8


  let flippedFunc = flip myFunc
  print (flippedFunc 5 10)
