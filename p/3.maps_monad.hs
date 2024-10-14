
import Data.Map as M

type Row = M.Map String String
type DB = [Row]


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


-- Function to lookup a value in a row by key
lookupValue :: String -> Row -> Maybe String
lookupValue key row = M.lookup key row

-- Function to find a value in a row with a default if not found
findWithDefaultValue :: String -> Row -> String -> String
findWithDefaultValue key row defaultValue = M.findWithDefault defaultValue key row

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



main :: IO ()
main = do
    let testRow = M.fromList [("Name", "Test"), ("Id", "0000000")]

    putStrLn "Original Row:"
    print testRow

    putStrLn "\nLookup for 'Name':"
    print $ lookupValue "Name" testRow

    putStrLn "\nFind with default for 'Age':"
    print $ findWithDefaultValue "Age" testRow "N/A"

    putStrLn "\nCheck if 'Id' exists in row:"
    print $ checkMember "Id" testRow

    putStrLn "\nInserting 'Age':"
    let updatedRow = insertValue "Age" "30" testRow
    print updatedRow

    putStrLn "\nDeleting 'Id':"
    let rowAfterDeletion = deleteKey "Id" updatedRow
    print rowAfterDeletion

    putStrLn "\nKeys in the row:"
    print $ getKeys rowAfterDeletion

    putStrLn "\nCreating a row from a list of key-value pairs:"
    let newRow = fromListToRow [("Color", "Red"), ("Shape", "Circle")]
    print newRow

    putStrLn "\nConverting a row to a list of key-value pairs:"
    print $ toListFromRow newRow

    putStrLn "\nUsing the 'eg' database:"
    print eg

    putStrLn "\nKeys of the first row in 'eg':"
    print $ getKeys (head eg)

    putStrLn "\nLookup for 'Name' in the first row of 'eg':"
    print $ lookupValue "Name" (head eg)

    putStrLn "\nFinding with default for 'Unknown' in the first row of 'eg':"
    print $ findWithDefaultValue "Unknown" (head eg) "Not Found"


    putStrLn "\nLookup for 'Name' in the first row of 'eg':"
    print $ find "Name" (head eg)



-- data List a = Nil | Cons a (List a)


-- return :: a -> M a
-- (>>=) :: m a -> (a -> m b) -> m b

-- return x = [x]
-- xs >>= f = concat (map f xs)

data Expr = Val Int | Div Expr Expr

-- datatype Expr
-- 2 constructors, Val and Div
-- expressions built up from integer values using division operator
--

-- 1            ->  val 1
-- 6 / 2        ->  Div (Val 6) (Val 2)
-- 6 / (3 / 1)  ->  Div (Val 6) ( Div (Val 3) (Val 1) )

-- diver (Div (Val 6) (Val 2))
diver :: Expr -> Int
diver (Val n) = n
diver (Div x y) = (diver x) `div` (diver y)


-- diver2 (Div (Val 6) (Val 2))
diver2 :: Expr -> Maybe Int
diver2 (Val n) = Just n
diver2 (Div x y) =
    case diver2 x of
        Nothing -> Nothing
        Just n -> case diver2 y of
            Nothing -> Nothing
            Just m -> safediv n m



  -- eval1 :: Expr -> Maybe Int
  -- eval1 (Val n) = return n
  -- eval1 (Div x y) = eval1 x >>= n ->
  --                   eval1 y >>= n ->
  --                   safediv n m))


-- THIS IS THE MONAD

-- eval (Div (Val 6) (Val 2))
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do
      n <- eval x
      m <- eval y
      safediv n m

-- Nothing is one of the constructors in the Maybe type
-- Just is one of the constructors in the Maybe type
safediv :: Int -> Int -> Maybe Int
safediv n m =
  if m == 0 then Nothing
else
  Just (n `div` m)

-- THIS IS ALSO THE MONAD
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = case m of
  Nothing -> Nothing
  Just x  -> f x

-- a monad is some kind of type constructor like maybe or list, anything
-- together with 2 functions that has types
-- return and
-- >>=

-- return (or pure) — to inject a value into a monad.
-- >>= (bind) — to chain monadic operations together.

-- bridge between pure world of values a and impure world Maybe a things that go wrong
-- return :: a -> Maybe a
-- sequences gives us: something that can fail like Maybe, give it a function what to do with that a if a fails, to get back a Maybe b
-- >>= :: Maybe a -> (a -> Maybe b) -> Maybe b


-- return x = [x]
-- xs >>= f = concat (map f xs)

-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
