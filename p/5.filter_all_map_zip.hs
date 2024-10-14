-- List and Number Utilities

-- Check if all elements are greater than a given value
isGr :: Int -> [Int] -> Bool
isGr s l = all (> s) l

-- Sample list
al :: [Int]
al = [1, 2, 3, 4, 5]


-- Function to check if an Integer is positive
isPositive :: Integer -> Bool
isPositive a = a > 0

-- Pretty-print a list of Integers
pp :: [Integer] -> String
pp [] = ""
pp [f] = show f
pp (f:l) = show f ++ " " ++ pp l

-- Check if any element in the list equals a given value
anyer :: [Int] -> Int -> Bool
anyer xs c = any (== c) xs  -- returns true if any element equals c

-- Check if all elements are less than a given value
aller :: [Int] -> Int -> Bool
aller xs c = all (< c) xs  -- returns true if all elements are less than c

-- Map a function over a list and add a constant
maper :: [Int] -> Int -> [Int]
maper xs c = map (+ c) xs  -- adds c to all elements

-- Filter elements of a list that are less than a given value
filterer :: [Int] -> Int -> [Int]
filterer xs c = filter (< c) xs  -- returns the portion of the list satisfying the condition

-- Check if a character is a digit
isDouble :: Char -> Bool
isDouble x = elem x "0123456789."

-- Sample list of integers
myList :: [Int]
myList = [3, 5, 1, 7, 9, 2, 8, 4, 6, 0, 12, 15, 11, 10, 14, 13, 19, 18, 17, 16]

-- Get the maximum element of a list
maxList :: (Ord a) => [a] -> a
maxList [] = error "Maximum of empty list"
maxList [x] = x
maxList (x:xs) = max x (maxList xs)

-- Custom maximum function
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

biggest :: [a] -> a
biggest a = maximum a

-- Replicate an element n times
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

-- Custom zip function
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- Custom take function
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

-- Custom reverse function
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]




-- Check if an element is in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- Check if two rows have the same length and all elements of the first are in the second
alEleRows :: [Int] -> [Int] -> Bool
alEleRows row1 row2 =
    let sameLength = length row1 == length row2
        allInRow2 = all (`elem` row2) row1
    in sameLength && allInRow2



-- Database structure
data DB = DB [[Int]]
    deriving (Show)

data Query = GetRow Int | CountRows | Validate | SumColumns
    deriving (Show)

data Transformer = DeleteRow Int | AddRow [Int] | Sort
    deriving (Show)

-- Database example
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

-- Sample rows for comparison
db1 :: DB
db1 =
    DB
        [ [1, 2075, 6, 1271, 12]
        , [2, 254, 239, 65, 571]
        , [3, 1510, 301, 1570, 703]
        , [4, 271, 2009, 2085, 1899]
        , [5, 1974, 449, 1975, 580]
        , [6, 1329, 13, 1152, 608]
        , [7, 2019, 724, 468, 953]
        , [8, 1404, 2155, 1671, 324]
        , [9, 1714, 1135, 738, 959]
        , [10, 1223, 1398, 1466, 876]
        ]


-- Sum the columns of the database
sumColumns :: DB -> [Int]
sumColumns (DB rows) =
    let numCols = length (head rows)
        zeros = replicate numCols 0
    in foldr (zipWith (+)) zeros rows

-- Alternative sumColumns function
sumColumns1 :: DB -> [Int]
sumColumns1 (DB rows) = foldr (zipWith (+)) zeros rows
    where
    numCols = length (head rows)
    zeros = replicate numCols 0

-- Check if two databases are equivalent
equivDB :: DB -> DB -> Bool
equivDB (DB db1) (DB db2) =
    length db1 == length db2 && all (\(row1, row2) -> row1 == row2) (zip db1 db2)



l1 :: [Integer]
l1 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
l2 :: [Integer]
l2 = [1, 1, 1, 1, 1, 1, 1, 1, 3]


main :: IO ()
main = do
    -- Test isGr function
    print $ isGr 0 al  -- Should return True

    -- Test column sums
    let cs = sumColumns db
    putStrLn $ "The sum of each column is: " ++ show cs

    let cs1 = sumColumns1 db
    putStrLn $ "The sum of each column (alternative) is: " ++ show cs1

    -- Test equivalence of databases
    let eq = equivDB db db1
    putStrLn $ "Are the two databases equivalent? " ++ show eq

    putStrLn $ "zip l1 l2 " ++ show (zipWith (+) l1 l2)

    print $ isGr 0 al  -- Should return True
