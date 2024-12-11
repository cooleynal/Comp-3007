module A5Solution where

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
-- A db is "valid" if all the rows have the same keys (according to M.keys).
type DB = [Row]

eg :: DB
eg =
  [ M.fromList [("Name", "Bingo"), ("Id", "1234567")],
    M.fromList [("Name", "Bongo"), ("Id", "8901234")],
    M.fromList [("Name", "Fungo"), ("Id", "0")],
    M.fromList [("Name", "Dingo"), ("Id", "5678901")]
  ]

sameKeys :: Row -> Row -> Bool
sameKeys m1 m2 =
  all (`elem` (M.keys m2)) (M.keys m1)
    && all (`elem` (M.keys m1)) (M.keys m2)

isValid :: DB -> Bool
isValid [] = True
isValid (row : rows) = all (sameKeys row) rows

-- deletes the rwo whose primary key has the given value
deleteRow :: DB -> String -> String -> DB
deleteRow db primaryKey value = deleteRow' [] db primaryKey value

-- Must be tail recursive.
deleteRow' :: DB -> DB -> String -> String -> DB
deleteRow' z [] pkey val = z
deleteRow' z (row : rows) pkey val | find pkey row == val = deleteRow' z rows pkey val
deleteRow' z (row : rows) pkey val = deleteRow' (row : z) rows pkey val

-- Get all rows whose primary key value is in the given set of strings.
getRows :: DB -> String -> [String] -> [Row]
getRows db primaryKey values = getRows' [] db primaryKey values

-- Must be tail recursive.
getRows' :: [Row] -> DB -> String -> [String] -> [Row]
getRows' z [] _ _ = z
getRows' z (row : rows) pkey vals
  | find pkey row `elem` vals =
      getRows' (row : z) rows pkey vals
getRows' z (row : rows) pkey vals =
  getRows' z rows pkey vals

-- return 1) the rows whose primary key value is in the given list of strings,
-- and 2) the db with those rows removed.
extractRows :: DB -> String -> [String] -> ([Row], DB)
extractRows db primaryKey values = extractRows' [] [] db primaryKey values

-- Must be tail recursive.
extractRows' :: [Row] -> DB -> DB -> String -> [String] -> ([Row], DB)
extractRows' zRows zDB [] pkey vals = (zRows, zDB)
extractRows' zRows zDB (row : rows) pkey vals
  | find pkey row `elem` vals =
      extractRows' (row : zRows) zDB rows pkey vals
extractRows' zRows zDB (row : rows) pkey vals =
  extractRows' zRows (row : zDB) rows pkey vals
