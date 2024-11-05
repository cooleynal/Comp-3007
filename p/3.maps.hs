import Data.Map as M
import Debug.Trace (trace)

-- Define a Row as a Map from String to String and a DB as a list of Rows
type Row = M.Map String String
type DB = [Row]

-- Example database
eg :: DB
eg =
  [ M.fromList [("Name", "Bingo"), ("Id", "1234567")]
  , M.fromList [("Name", "Bongo"), ("Id", "8901234")]
  , M.fromList [("Name", "Fungo"), ("Id", "0")]
  , M.fromList [("Name", "Dingo"), ("Id", "5678901")]
  , M.fromList [("Name", "Zingo"), ("Id", "1238904")]
  , M.fromList [("Name", "Mingo"), ("Id", "5555555")]
  , M.fromList [("Name", "Ringo"), ("Id", "7777777")]
  , M.fromList [("Name", "Pingo"), ("Id", "8888888")]
  , M.fromList [("Name", "Kingo"), ("Id", "9999999")]
  , M.fromList [("Name", "Tingo"), ("Id", "1111111")]
  , M.fromList [("Name", "Fingo"), ("Id", "2222222")]
  , M.fromList [("Name", "Lingo"), ("Id", "3333333")]
  , M.fromList [("Name", "Singo"), ("Id", "4444444")]
  , M.fromList [("Name", "Wingo"), ("Id", "1234568")]
  , M.fromList [("Name", "Dingo Jr."), ("Id", "5678902")]
  , M.fromList [("Name", "Fungo Jr."), ("Id", "0")]
  , M.fromList [("Name", "Bingo Jr."), ("Id", "1234569")]
  , M.fromList [("Name", "Bongo Jr."), ("Id", "8901235")]
  , M.fromList [("Name", "Ringo Jr."), ("Id", "7777778")]
  , M.fromList [("Name", "Kingo Sr."), ("Id", "9999998")]
  , M.fromList [("Name", "Mingo Sr."), ("Id", "5555554")]
  , M.fromList [("Name", "Zingo Jr."), ("Id", "1238905")]
  ]

-- Function to look up a value in a row by key
lookupValue :: String -> Row -> Maybe String
lookupValue key row = M.lookup key row

-- Function to find a value in a row with a default if not found
findWithDefaultValue :: String -> Row -> String -> String
findWithDefaultValue key row defaultValue = M.findWithDefault defaultValue key row

-- Alias for finding a value with a default of ""
find :: String -> Row -> String
find = M.findWithDefault ""

-- Function to check if a key exists in a row
checkMember :: String -> Row -> Bool
checkMember key row = M.member key row

-- Function to insert a new key-value pair into a row
insertValue :: String -> String -> Row -> Row
insertValue key value row = M.insert key value row

-- Function to delete a key from a row
deleteKey :: String -> Row -> Row
deleteKey key row = M.delete key row

-- Function to get all keys in a row
getKeys :: Row -> [String]
getKeys row = M.keys row

-- Function to convert a list of key-value pairs to a row
fromListToRow :: [(String, String)] -> Row
fromListToRow = M.fromList

-- Function to convert a row to a list of key-value pairs
toListFromRow :: Row -> [(String, String)]
toListFromRow = M.toList

-- Function to check if two rows have the same keys
sameKeys :: Row -> Row -> Bool
sameKeys m1 m2 =
  all (`elem` (M.keys m2)) (M.keys m1) && all (`elem` (M.keys m1)) (M.keys m2)

-- Function to check if a database is valid (all rows have the same keys)
isValid :: DB -> Bool
isValid [] = True
isValid (fr : rest) = all (sameKeys fr) rest

-- Function to delete the row whose primary key has the given value
deleteRow :: DB -> String -> String -> DB
deleteRow db primaryKey value = deleteRow' [] db primaryKey value

-- Tail-recursive helper function for deleteRow
deleteRow' :: DB -> DB -> String -> String -> DB
deleteRow' dba [] _ _ = reverse dba
deleteRow' dba (dbh : dbr) k v
  | Just v_row <- M.lookup k dbh, v_row == v = deleteRow' dba dbr k v
  | otherwise = deleteRow' (dbh : dba) dbr k v

-- Function to get all rows whose primary key value is in the given set of strings
getRows :: DB -> String -> [String] -> [Row]
getRows db primaryKey values = getRows' [] db primaryKey values

-- Tail-recursive helper function for getRows
getRows' :: [Row] -> DB -> String -> [String] -> [Row]
getRows' dba [] _ _ = reverse dba
getRows' dba (dbh : dbr) k vs
  | Just v <- M.lookup k dbh, v `elem` vs = getRows' (dbh : dba) dbr k vs
  | otherwise = getRows' dba dbr k vs

-- Function to extract rows and return them along with the modified DB
extractRows :: DB -> String -> [String] -> ([Row], DB)
extractRows db primaryKey values = extractRows' [] [] db primaryKey values

-- Tail-recursive helper function for extractRows
extractRows' :: [Row] -> DB -> DB -> String -> [String] -> ([Row], DB)
extractRows' rowa dba [] _ _ = (rowa, dba)
extractRows' rowa dba (dbh:dbr) k ks
  | Just v <- M.lookup k dbh, v `elem` ks = extractRows' (dbh : rowa) dba dbr k ks
  | otherwise = extractRows' rowa (dbh : dba) dbr k ks

-- Function to get a row by index
getRow :: Int -> DB -> Row
getRow index db = db !! index





main :: IO ()
main = do



  putStrLn "Checking if the example database is valid:"
  print $ isValid eg
  putStrLn ""

  putStrLn "Deleting row with Id '8901234':"
  print $ deleteRow eg "Id" "8901234"
  putStrLn ""

  putStrLn "Getting rows with Ids '1234567' and '8901234':"
  print $ getRows eg "Id" ["1234567", "8901234"]
  putStrLn ""

  putStrLn "Extracting rows with Ids '5678901' and '8901234':"
  print $ extractRows eg "Id" ["5678901", "8901234"]
  putStrLn ""

  let sr1 = getRows eg "Id" ["8901234"]
  let sr2 = getRows eg "Id" ["8901234"]

  putStrLn "Getting rows 'sr1' and 'sr2':"
  print sr1
  print sr2
  putStrLn ""

  putStrLn "Checking keys of the first row in the example database:"
  print $ getKeys (head eg)

  putStrLn "Lookup for 'Name' in the first row of the example database:"
  print $ lookupValue "Name" (head eg)

  putStrLn "Finding with default for 'Unknown' in the first row of the example database:"
  print $ findWithDefaultValue "Unknown" (head eg) "Not Found"


  -- -- Print the example list results
  -- putStrLn "Example with `do` notation:"
  -- print exampleList

  -- putStrLn "Example without `do` notation:"
  -- print exampleList'
