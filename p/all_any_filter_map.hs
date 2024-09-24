-- data MyList = Empty | Cons String MyList
--   deriving (Show)

-- firstElement :: MyList -> String
-- firstElement Empty = ""  -- Return an empty string if the list is empty
-- firstElement (Cons x _) = x  -- Return the first element if the list is non-empty

-- myEmptyList :: MyList
-- myEmptyList = Empty

-- myList :: MyList
-- myList = Cons "Hello" (Cons "World" Empty)

-- data DB = DB [[Int]] deriving (Show)

-- db :: DB
-- db =
--   DB
--     [ [1, 2075, 6, 1271, 1930]
--     , [2, 254, 239, 65, 571]
--     , [3, 1510, 301, 1570, 703]
--     , [4, 271, 2009, 2085, 1899]
--     , [5, 1974, 449, 1975, 580]
--     , [6, 1329, 1970, 1152, 608]
--     , [7, 2019, 724, 468, 953]
--     , [8, 1404, 2155, 1671, 324]
--     , [9, 1714, 1135, 738, 959]
--     , [10, 1223, 1398, 1466, 876]
--     ]


-- -- ghci> printDB db
-- printDB :: DB -> IO ()
-- printDB (DB rows) = printRows rows
--   where
--     printRows [] = return ()          -- Base case: empty list of rows
--     printRows (row:rest) = do
--       printRow row                    -- Print the current row
--       putStrLn ""                     -- Print a newline after each row
--       printRows rest                  -- Recur with the remaining rows

--     printRow [] = return ()           -- Base case: empty row
--     printRow (x:xs) = do
--       putStr (show x ++ " ")          -- Print the current element followed by a space
--       printRow xs


-- dd :: [Integer]
-- dd = [3, 1510, 301, 1570, 703]


-- manualSum :: [Integer] -> Integer
-- manualSum [] = 0
-- manualSum (x:xs) = x + (manualSum xs)



-- tf (-5)
tf :: Integer -> Bool
tf a
  | a > 0     = True
  | a < 0     = False
  | otherwise = False


pp:: [Integer] -> String
pp [] = ""
pp [f] = show f
pp (f:l) = show f ++ " " ++ (pp l)



-- anyer [1, 2, 3] 2
-- True
anyer :: [Int] -> Int -> Bool
anyer [] _ = False
anyer xs c = any (==c) xs -- returns true if any element is less than c


-- aller [1, 2, 3] 2
aller :: [Int] -> Int -> Bool
aller [] _ = False
aller xs c = all (< c) xs -- returns true if all elements satisfy condition

-- maper [1, 2, 3] 2
-- [3,4,5]
maper :: [Int] -> Int -> [Int]
maper [] _ = []
maper xs c = map (+c) xs -- adds c to all elements

-- filterer [1, 2, 3, 4, 5] 4
-- [1,2,3]
filterer :: [Int] -> Int -> [Int]
filterer [] _ = []
filterer xs c = filter (<c) xs -- returns the portion of the list satisfying the condition
