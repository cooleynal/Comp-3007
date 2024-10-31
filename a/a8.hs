-- Assignment 8
--
-- In this assignment you will implement a simple database user interface. The
-- interface will be generic, working for any type that is an instance of the
-- DB type class. In addition to getting some sort-of practical experience with
-- type classes, you'll also get more experience with IO in Haskell.

-- module GraderBase where



type PrimaryKey = String

type ColName = String

-- Types that support some basic database operations. The operations assume all
-- values are strings. Instances will need to convert as needed.
class DB a where
  empty :: a
  query :: a -> PrimaryKey -> ColName -> String -- "" if key not present
  update :: a -> PrimaryKey -> ColName -> String -> a -- db unchanged if key not found
  addRow :: a -> [String] -> a -- assume row's key not already present in db




-- Important: the autograder assumes you are using this for all prompting for user input
prompt :: String -> IO String
prompt str = do
  putStrLn ("[" ++ str ++ "] ")
  getLine

-- Prompt for a command, collect extra input if needed.
-- Commands and required actions:
--   query   prompt twice, once for a key and once for a column name
--   update  prompt three times: key, column name, new value
--   addRow  prompt once for a row, where row values are separated by spaces
--   quit
--   print   print the current db using the built-in function "print"
-- The files a8-input.txt and a8-output.txt give an example. The first file gives the
-- lines a user would input, and the second gives the collected output, which
-- includes all the prompts.

dbUI :: (DB a, Show a) => a -> IO ()
dbUI db = do

  command <- prompt "Enter command"

  case command of

    "query" -> do
      key <- prompt "Primary key"
      col <- prompt "Column name"
      putStrLn $ "Query result: " ++ query db key col
      dbUI db

    "update" -> do
      key <- prompt "Primary key"
      col <- prompt "Column name"
      nv <- prompt "New value"
      let newDb = update db key col nv
      putStrLn "Row updated."
      dbUI newDb

    "addRow" -> do
      rowValues <- prompt "input row values separated by spaces"
      let row = words rowValues
      if length row == 3 then
        do
          let newRow = addRow db row
          putStrLn "row added."
          dbUI newRow
      else
        putStrLn "invalid: expected 3 values (name, age, accountBalance). NOOP"

    "print" -> do
      putStrLn (show db)
      dbUI db

    "empty" -> do
      putStrLn "database has been reset to empty."
      let newDb = empty :: Customers
      dbUI newDb

    "quit" -> putStrLn "quiting"

    _ -> do
      putStrLn "unknown operation"
      dbUI db

-- A particular database to test your UI with.
data Customers = Customers [Customer]

data Customer = Customer
  { name :: String, -- primary key
    age :: Int,
    accountBalance :: Double
  }
  deriving (Show)

-- Puts a newline after each customer
instance Show Customers where
  show (Customers cs) = concatMap ((++ "\n") . show) cs

-- Safe version of head.
hd :: [a] -> Maybe a
hd (x : l) = Just x
hd _ = Nothing

-- Safe version of tail.
tl :: [a] -> Maybe [a]
tl (x : l) = Just l
tl _ = Nothing

-- First member of a list to satisfy a predicate
findBy :: (a -> Bool) -> [a] -> Maybe a
findBy p = hd . filter p

-- The value of a maybe if there is one, else a default value
maybeOr :: Maybe a -> a -> a
maybeOr (Just x) _ = x
maybeOr _ x = x

-- added cause i dont like \c -> anon functions yet
isMatchingKey :: String -> Customer -> Bool
isMatchingKey key c = name c == key


instance DB Customers where
  empty = Customers []


  query (Customers []) _ _ = "" -- clears non exhaustive

  -- query (Customers (c:cs)) key col =
  --   if name c == key then
  --     case col of
  --       "age" -> show (age c)
  --       "accountBalance" -> show (accountBalance c)
  --       _ -> "Invalid column name"
  --   else
  --     query (Customers cs) key col

  -- query (Customers cs) key col =
  -- case findBy (\c -> name c == key) cs of
  --   Just c -> case col of
  --     "age" -> show (age c)
  --     "accountBalance" -> show (accountBalance c)
  --     _ -> "Invalid column name"
  --   Nothing -> ""

  query (Customers cs) key col =
    case findBy (isMatchingKey key) cs of
      Just c -> case col of
        "age" -> show (age c)
        "accountBalance" -> show (accountBalance c)
        _ -> "Invalid column name"
      Nothing -> ""


  -- update (Customers cs) key col nv =
  --   case findBy (isMatchingKey key) cs of
  --     Just c -> case col of
  --       "age" -> show (age c)
  --       "accountBalance" -> show (accountBalance c)
  --       _ -> "Invalid column name"
  --     Nothing -> ""


  -- update (Customers cs) key col nv =
  --   Customers (map updateCustomer cs)
  --   where
  --     updateCustomer customer
  --       | name customer == key =
  --           case col of
  --             "age" -> customer { age = read nv }
  --             "accountBalance" -> customer { accountBalance = read nv }
  --             _ -> customer
  --       | otherwise = customer

  -- update :: Customers -> PrimaryKey -> ColName -> String -> Customers
  -- update (Customers cs) key col newValue =
  --   case findBy (isMatchingKey key) cs of
  --     Just c  -> Customers (map (applyUpdate c) cs)
  --     Nothing -> Customers cs

  --   where
  --     applyUpdate customer c
  --       | name c == name customer =
  --           case col of
  --             "age"             -> c { age = read newValue :: Int }
  --             "accountBalance"  -> c { accountBalance = read newValue :: Double }
  --             _                 -> c
  --       | otherwise = c

  update (Customers cs) key col newValue =
    case findBy (isMatchingKey key) cs of
      Just customer -> Customers (updatedCustomer : otherCustomers)
        where
          updatedCustomer = case col of
            "age" -> customer { age = read newValue :: Int }
            "accountBalance" -> customer { accountBalance = read newValue :: Double }
            _ -> customer

          otherCustomers = filter (not . isMatchingKey key) cs  -- readd rest
      Nothing -> Customers cs



  addRow (Customers cs) row =
    let
      name = row !! 0 :: String
      age = read (row !! 1) :: Int
      balance = read (row !! 2) :: Double
      newCustomer = Customer name age balance
    in Customers (newCustomer : cs)

db :: Customers
db = empty


main :: IO ()
main = dbUI db
