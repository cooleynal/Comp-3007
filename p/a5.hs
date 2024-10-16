-- Assignment 5
-- Due Monday Sept 7 23:59
--
-- Do not change/delete any of the supplied code except for the lines with
-- "undefined".

-- important functions from Map:
-- lookup,
-- findWithDefault,
-- member,
-- insert,
-- delete,
-- keys,
-- fromList,
-- toList.

-- Note: to use these, preface with "M." e.g.
-- M.lookup.
import Debug.Trace (trace)
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

-- sameKeys :: Row -> Row -> Bool
-- sameKeys r1 r2 =
--   length (M.keys r1) == length (M.keys r2) &&
--   all (== True) (zipWith (==) (M.keys r1) (M.keys r2))

sameKeys :: Row -> Row -> Bool
sameKeys m1 m2 =
  all (`elem` (M.keys m2)) (M.keys m1) && all (`elem` (M.keys m1)) (M.keys m2)


-- A db is "valid" if all the rows have the same keys (according to M.keys).
isValid :: DB -> Bool
isValid [] = True
isValid (fr : rest) = all (sameKeys fr) rest

-- deletes the row whose primary key has the given value
          -- db / pri key / value / return db
deleteRow :: DB -> String -> String -> DB
deleteRow db primaryKey value = deleteRow' [] db primaryKey value


-- printDB $ deleteRow eg "Id" "8901234"
-- Must be tail recursive.
deleteRow' :: DB -> DB -> String -> String -> DB
deleteRow' dba [] _ _ = dba
deleteRow' dba (dbh : dbr) k v
  | Just v_row <- M.lookup k dbh, v_row == v = deleteRow' dba dbr k v
  | otherwise = deleteRow' (dbh : dba) dbr k v



deleteRow1 :: DB -> String -> String -> DB
deleteRow1 db primaryKey value = deleteRow1' [] db primaryKey value

deleteRow1' :: DB -> DB -> String -> String -> DB
deleteRow1' dbacc [] _ _ = dbacc
deleteRow1' dbacc (row : rest) k v
  | find k row == v   = deleteRow1' dbacc rest k v
  | otherwise         = deleteRow1' (row : dbacc) rest k v



deleteRow2 :: DB -> String -> String -> DB
deleteRow2 db primaryKey value = deleteRow2' [] db primaryKey value

-- deleteRow2' :: DB -> DB -> String -> String -> DB
-- deleteRow2' dbacc [] _ _ = dbacc
-- deleteRow2' dbacc (row : rest) k v =
--   -- trace ("Checking row: " ++ show row ++ ", with key: " ++ k ++ " and value: " ++ v ++ "\n") $
--     if findKeyBool k row
--       -- then trace ("Row matches, deleting: " ++ show row) $
--            deleteRow2' dbacc rest k v
--     else
--       -- trace ("Row does not match, keeping: " ++ show row) $
--           deleteRow2' (row : dbacc) rest k v
--   where
--     findKeyBool :: String -> Row -> Bool
--     findKeyBool value row = M.member value row


deleteRow2' :: DB -> DB -> String -> String -> DB
deleteRow2' dbacc [] _ _ = reverse dbacc
deleteRow2' dbacc (row : rest) k v =
    if M.lookup k row == Just v
    then deleteRow2' dbacc rest k v
    else deleteRow2' (row : dbacc) rest k v

    -- where
    --   findKeyBool :: String -> Row -> Bool
    --   findKeyBool value row = M.member value row


deleteRow3 :: DB -> String -> String -> DB
deleteRow3 db primaryKey value = deleteRow3' [] db primaryKey value

deleteRow3' :: DB -> DB -> String -> String -> DB
deleteRow3' dbacc [] _ _ = reverse dbacc
deleteRow3' dbacc (row : rest) k v =
    if  M.lookup k row == Just v
      then deleteRow3' dbacc rest k v
    else deleteRow3' (row : dbacc) rest k v
    -- where
    --   findKeyBool :: String -> Row -> Bool
    --   findKeyBool value row = M.member value row



findKey :: String -> Row -> String
findKey value row =
    case M.toList row of
      [] -> ""
      xs -> case M.lookup value (M.fromList xs) of
              Just k  -> k
              Nothing -> ""




-- Get all rows whose primary key value is in the given set of strings.
getRows :: DB -> String -> [String] -> [Row]
getRows db primaryKey values = getRows' [] db primaryKey values

-- Must be tail recursive.
getRows' :: [Row] -> DB -> String -> [String] -> [Row]
getRows' dba [] _ _ = reverse dba
getRows' dba (dbh : dbr) k vs
  | Just v <- M.lookup k dbh, v `elem` vs = getRows' (dbh : dba) dbr k vs
  | otherwise = getRows' dba dbr k vs


getRows1 :: DB -> String -> [String] -> [Row]
getRows1 db primaryKey values = getRows' [] db primaryKey values


getRows1' :: [Row] -> DB -> String -> [String] -> [Row]
getRows1' dbacc [] _ _ = dbacc
getRows1' dbacc (dbh : dbr) k vs
  | find k dbh `elem` vs  = getRows1' (dbh : dbacc) dbr k vs
  | otherwise             = getRows1' dbacc dbr k vs



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




extractRows1 :: DB -> String -> [String] -> ([Row], DB)
extractRows1 db primaryKey values = extractRows' [] [] db primaryKey values

-- Must be tail recursive.
extractRows1' :: [Row] -> DB -> DB -> String -> [String] -> ([Row], DB)
extractRows1' dbacc zDB [] k vs = (dbacc, zDB)
extractRows1' dbacc zDB (row : rows) k vs
  | find k row `elem` vs   = extractRows1' (row : dbacc) zDB rows k vs
  | otherwise                   = extractRows1' dbacc (row : zDB) rows k vs


getRow :: Int -> DB -> Row
getRow index db = db !! index



main :: IO ()
main = do

  print $ isValid eg
  putStrLn ""

  print $ (deleteRow eg "Id" "8901234")

  print $ getRows eg "Id" ["1234567", "8901234"]
  putStrLn ""

  print $ extractRows eg "Id" ["5678901", "8901234"]
  putStrLn ""

  let sr1 = getRows eg "Id" ["8901234"]
  let sr2 = getRows eg "Id" ["8901234"]

  print $ sr1
  print $ sr2


  print $ sameKeys (getRow 1 eg) (getRow 1 eg)
  putStrLn ""

  print $ deleteRow eg "Id" "8901234"
  putStrLn ""

  print $ deleteRow1 eg "Id" "8901234"
  putStrLn ""
  -- putStrLn "sss"
  print $ deleteRow2 eg "Id" "8901234"
  putStrLn ""

  print $ deleteRow3 eg "Id" "8901234"
  putStrLn ""


  print $ getRows1 eg "Id" ["8901234", "89012134"]
  putStrLn ""

  print $ extractRows eg "Id" ["8901234"]
  putStrLn ""


  print $ extractRows1 eg "Id" ["8901234"]
  putStrLn ""

  -- print ""

  -- print $ concat eg3
  -- print ""

  -- print $ length eg1
  -- print ""

  -- print $ sumColumns (length eg3) eg3
  -- print ""

  -- print $ tail eg3
  -- print ""
  -- ("Id", "1234567")
  -- , M.fromList [("Name", "Bongo"), ("Id", "8901234")]
  -- , M.fromList [("Name", "Fungo"), ("Id", "0")]
  -- , M.fromList [("Name", "Dingo"), ("Id", "5678901")]
