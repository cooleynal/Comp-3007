
import Data.Map qualified as M

-- stirling 15 8
-- stirling 55 2
stirling :: Int -> Int -> Int
stirling n 1 = 1
stirling 0 0 = 1
stirling n 0 = 0
stirling n k
  | n < k     = 0
  | otherwise = stirling (n - 1) (k - 1) + k * stirling (n - 1) k

f :: Integer -> Integer
f 0 = 1
f n = n * f (n - 1)

g :: Integer -> Float
g n = fromIntegral (2* n^2 - n) /6

results :: [Float]
results = [g n | n <- [1..100]]

r :: [Float]
r = [n | n <- [1..100]]


  -- Do not change/delete any of the supplied code except for the lines with
-- "undefined".

-- important functions from Map: lookup, findWithDefault, member, insert,
-- delete, keys, fromList, toList. Note: to use these, preface with "M." e.g.
-- M.lookup.


-- A row is a map from "column names", or "keys", to values.
type Row = M.Map String String

-- Handy definition.
find :: String -> Row -> String
find = M.findWithDefault ""

-- E.g. a data base with columns Name and Id and two rows (using a fake map
-- notation):
-- [ {"Name":"Bingo", "Id":"1234567"},
--   {"Name":"Bongo", "Id":"8901234"}
-- ]

-- A db may have a "primary key". This is just a distinguished column name.
-- Functions needing to know the primary key will take the column name as an
-- input.

type DB = [Row]


-- row1 = eg !! 0  -- First row
-- row2 = eg !! 1  -- Second row
-- getRow 1 eg
getRow :: Int -> DB -> Row
getRow index db = db !! index


printDB :: DB -> IO ()
printDB [] = return ()
printDB (row:rows) = do
    print row
    printDB rows


eg :: DB
eg =
  [ M.fromList [("Name", "Bingo"), ("Id", "1234567")]
  , M.fromList [("Name", "Bongo"), ("Id", "8901234")]
  , M.fromList [("Name", "Fungo"), ("Id", "0")]
  , M.fromList [("Name", "Dingo"), ("Id", "5678901")]
  ]

eg1 :: DB
eg1 =
  [ M.fromList [("Name", "Bingo"), ("Id", "1234567")]
  , M.fromList [("Name1", "Bongo"), ("Id", "8901234")]
  , M.fromList [("Name", "Fungo"), ("Id", "0")]
  , M.fromList [("Name", "Dingo"), ("Id", "5678901")]
  ]

-- M.keys $  getRow 1 eg

-- M.lookup "Id"( getRow 1 eg)
-- sameKeys (getRow 1 eg) (getRow 1 eg)
sameKeys :: Row -> Row -> Bool
-- sameKeys r1 r2 = all (True) ( zipWith (==) M.keys r1 M.keys r2)
sameKeys r1 r2 =
    length (M.keys r1) == length (M.keys r2) &&
    all (== True) (zipWith (==) (M.keys r1) (M.keys r2))
-- 1) add sort

-- M.keys $  getRow 1 eg
-- ["Id","Name"]

-- sameKeys (getRow 1 eg) (getRow 2 eg)

-- A db is "valid" if all the rows have the same keys (according to M.keys).
-- isValid eg1
isValid :: DB -> Bool
isValid [] = True
isValid (firstRow : rest) = all (sameKeys firstRow) rest -- suspect incorrect, should fix firstRow


-- printDB $ deleteRow eg "Id" "8901234"
-- printDB $ deleteRow eg "Id" "0"
-- deletes the row whose primary key has the given value
deleteRow :: DB -> String -> String -> DB
deleteRow db primaryKey value = deleteRow' [] db primaryKey value
-- deleteRow dba db primaryKey value = dba

-- Must be tail recursive.
deleteRow' :: DB -> DB -> String -> String -> DB
deleteRow' dba [] _ _ = dba                                   -- when the dbr is empty, return dba
deleteRow' dba (dbh : dbr) k v
  | Just v_row <- M.lookup k dbh, v_row == v = deleteRow' dba dbr k v
  -- | otherwise                = deleteRow' (dbh : dba) dbr k v -- append db head onto dbadder
  | otherwise = deleteRow' (dbh : dba) dbr k v


-- Get all rows whose primary key value is in the given set of strings.

-- printDB $ getRows eg "Id" ["1234567", "8901234"]
-- printDB $ getRows eg "Id" ["123457", "890123"]
getRows :: DB -> String -> [String] -> [Row]
getRows db primaryKey values = getRows' [] db primaryKey values

-- Must be tail recursive.
getRows' :: [Row] -> DB -> String -> [String] -> [Row]
getRows' dba [] _ _ = reverse dba
getRows' dba (dbh:dbr) k vs
  | Just v <- M.lookup k dbh, v `elem` vs = getRows' (dbh : dba) dbr k vs
  | otherwise = getRows' dba dbr k vs

-- M.lookup "Id"( getRow 1 eg)


-- return 1) the rows whose primary key value is in the given list of strings,
--    and 2) the db with those rows removed.
-- extractRows eg "Id" ["a", "b"]
-- extractRows eg "Id" ["8901234"]
-- extractRows eg "Id" ["5678901"]
-- extractRows eg "Id" ["5678901", "8901234"]
extractRows :: DB -> String -> [String] -> ([Row], DB)
extractRows db primaryKey values = extractRows' [] [] db primaryKey values



-- Must be tail recursive.
-- extractRows' :: [Row] -> DB -> DB -> String -> [String] -> ([Row], DB)
-- extractRows' rowa dba [] _ _ = (rowa, dba)
-- extractRows' rowa dba (dbh:dbr) k ks
--   | Just v <- M.lookup k dbh, v `elem` ks = extractRows' rowa (dbh : dba) dbr k ks
--   | otherwise = extractRows' (dbh : rowa) dba dbr k ks

-- words = ["hi", "rawr", "dee"]
-- "hi" `elem` words

extractRows' :: [Row] -> DB -> DB -> String -> [String] -> ([Row], DB)
extractRows' rowa dba [] _ _ = (rowa, dba)
extractRows' rowa dba (dbh:dbr) k ks
  | Just v <- M.lookup k dbh, v `elem` ks = extractRows' (dbh : rowa) dba dbr k ks
  | otherwise = extractRows' rowa (dbh : dba) dbr k ks


-- eg :: DB
-- eg =
--   [ M.fromList [("Name", "Bingo"), ("Id", "1234567")]
--   , M.fromList [("Name", "Bongo"), ("Id", "8901234")]
--   , M.fromList [("Name", "Fungo"), ("Id", "0")]
--   , M.fromList [("Name", "Dingo"), ("Id", "5678901")]
--   ]



-- ghci> extractRows eg "Id" ["5678901", "8901234"]
-- ([fromList [("Id","0"),("Name","Fungo")],fromList [("Id","1234567"),("Name","Bingo")]],

-- [fromList [("Id","5678901"),("Name","Dingo")],fromList [("Id","8901234"),("Name","Bongo")]])])