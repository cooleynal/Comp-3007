-- Assignment 8
--
-- In this assignment you will implement a simple database user interface. The
-- interface will be generic, working for any type that is an instance of the
-- DB type class. In addition to getting some sort-of practical experience with
-- type classes, you'll also get more experience with IO in Haskell.

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
  cmd <- prompt "Enter command"
  case cmd of
    "query" -> do
      doQuery db
      dbUI db
    "update" -> do
      db' <- doUpdate db
      dbUI db'
    "addRow" -> do
      db' <- doAddRow db
      dbUI db'
    "quit" -> do
      return ()
    "print" -> do
      print db
      dbUI db
    _ -> do
      putStrLn $ "Unrecognized command: " ++ cmd
      dbUI db

doQuery :: (DB a) => a -> IO ()
doQuery db = do
  key <- prompt "Primary key"
  col <- prompt "Column name"
  putStrLn $ "Query result: " ++ query db key col

doUpdate :: (DB a) => a -> IO a
doUpdate db = do
  key <- prompt "Primary key"
  col <- prompt "Column name"
  newVal <- prompt "New value"
  return $ update db key col newVal

doAddRow :: (DB a) => a -> IO a
doAddRow db = do
  response <- prompt "Input row values separated by spaces"
  return $ addRow db (words response)


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

instance DB Customers where
  empty = Customers []

  query (Customers cs) key colName =
    maybeOr result ""
    where
      result = do
        row <- findBy ((== key) . name) cs
        case colName of
          "age" -> return $ show $ age row
          "accountBalance" -> return $ show $ accountBalance row

  update (Customers cs) key colName newVal =
    Customers $ map updateRow cs
    where
      updateRow r = case colName of
        "age" -> r {age = read newVal :: Int}
        "accountBalance" -> r {accountBalance = read newVal :: Double}

  addRow (Customers cs) [key, a, b] =
    Customers $ Customer {name = key, age = read a :: Int, accountBalance = read b :: Double} : cs


db :: Customers
db = empty

main :: IO ()
main = dbUI db
