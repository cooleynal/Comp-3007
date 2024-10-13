-- Assignment 5
-- Due Monday Sept 7 23:59
--
-- Do not change/delete any of the supplied code except for the lines with
-- "undefined".

-- important functions from Map: lookup, findWithDefault, member, insert,
-- delete, keys, fromList, toList. Note: to use these, preface with "M." e.g.
-- M.lookup.
import Data.Map as M

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

eg :: DB
eg =
  [ M.fromList [("Name", "Bingo"), ("Id", "1234567")]
  , M.fromList [("Name", "Bongo"), ("Id", "8901234")]
  , M.fromList [("Name", "Fungo"), ("Id", "0")]
  , M.fromList [("Name", "Dingo"), ("Id", "5678901")]
  ]

sameKeys :: Row -> Row -> Bool
sameKeys r1 r2 =
  length (M.keys r1) == length (M.keys r2) &&
  all (== True) (zipWith (==) (M.keys r1) (M.keys r2))


-- A db is "valid" if all the rows have the same keys (according to M.keys).
isValid :: DB -> Bool
isValid [] = True
isValid (fr : rest) = all (sameKeys fr) rest

-- deletes the row whose primary key has the given value
deleteRow :: DB -> String -> String -> DB
deleteRow db primaryKey value = deleteRow' [] db primaryKey value


-- printDB $ deleteRow eg "Id" "8901234"
-- Must be tail recursive.
deleteRow' :: DB -> DB -> String -> String -> DB
deleteRow' dba [] _ _ = dba
deleteRow' dba (dbh : dbr) k v
  | Just v_row <- M.lookup k dbh, v_row == v = deleteRow' dba dbr k v
  | otherwise = deleteRow' (dbh : dba) dbr k v


-- Get all rows whose primary key value is in the given set of strings.
getRows :: DB -> String -> [String] -> [Row]
getRows db primaryKey values = getRows' [] db primaryKey values

-- Must be tail recursive.
getRows' :: [Row] -> DB -> String -> [String] -> [Row]
getRows' dba [] _ _ = reverse dba
getRows' dba ( dbh : dbr) k vs
  | Just v <- M.lookup k dbh, v `elem` vs = getRows' (dbh : dba) dbr k vs
  | otherwise = getRows' dba dbr k vs

-- return 1) the rows whose primary key value is in the given list of strings,
-- and 2) the db with those rows removed.
extractRows :: DB -> String -> [String] -> ([Row], DB)
extractRows db primaryKey values = extractRows' [] [] db primaryKey values

-- Must be tail recursive.
extractRows' :: [Row] -> DB -> DB -> String -> [String] -> ([Row], DB)
extractRows' rowa dba [] _ _ = (rowa, dba)
extractRows' rowa dba (dbh:dbr) k ks
  | Just v <- M.lookup k dbh, v `elem` ks = extractRows' (dbh : rowa) dba dbr k ks
  | otherwise = extractRows' rowa (dbh : dba) dbr k ks


main :: IO ()
main = do

  print $ isValid eg
  print ""

  print $ (deleteRow eg "Id" "8901234")

  print $ getRows eg "Id" ["1234567", "8901234"]
  -- print ""

  print $ extractRows eg "Id" ["5678901", "8901234"]
  -- print ""

  -- print $ append eg eg1
  -- print ""

  -- print $ concat eg3
  -- print ""

  -- print $ length eg1
  -- print ""

  -- print $ sumColumns (length eg3) eg3
  -- print ""

  -- print $ tail eg3
  -- print ""
