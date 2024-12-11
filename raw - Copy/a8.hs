-- Assignment 8
--
-- In this assignment you will implement a simple database user interface. The
-- interface will be generic, working for any type that is an instance of the
-- DB type class. In addition to getting some sort-of practical experience with
-- type classes, you'll also get more experience with IO in Haskell.

import GraderBase

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
  undefined

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
  empty = undefined
  query = undefined
  update = undefined
  addRow = undefined

db :: Customers
db = empty

main :: IO ()
main = dbUI db
