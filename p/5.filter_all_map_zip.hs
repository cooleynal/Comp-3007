

al :: [Int]
al = [1, 2, 3, 4, 5]

isGr :: Int -> [Int] -> Bool
isGr _ [] = False
isGr s l = all (>s) l


main :: IO ()
main = do
    print $ isGr 0 al



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

-- all (`elem` [1, 2]) [1, 2, 3]
-- elem 2 [1, 2, 3]

isDouble :: Char -> Bool
isDouble x = elem x "0123456789."




myList :: [Int]
myList = [3, 5, 1, 7, 9, 2, 8, 4, 6, 0, 12, 15, 11, 10, 14, 13, 19, 18, 17, 16]


-- Ord is a type of function that has to be ordered
maxList :: (Ord a) => [a] -> a
maxList [] = error "Maximum of empty list"
maxList [x] = x
maxList (x:xs) = max x (maxList xs)



-- maximum myList
-- 19
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)


-- replicate 3 5
-- [5,5,5]
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x


-- zip [1,2,3] [2,3]
-- [(1,2),(2,3)] # truncates
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


-- take' 4 myList
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- reverse myList
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- quicksort myList
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


-- check if element in list
-- elem' 1 myList
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs



alEleRows :: [Int] -> [Int] -> Bool
alEleRows row1 row2 =
    let
        sameLength = length row1 == length row2
        allInRow2 = all (`elem` row2) row1
    in
        sameLength && allInRow2



data DB = DB [[Int]]
    deriving (Show)

data Query = GetRow Int | CountRows | Validate | SumColumns
    deriving (Show)

data Transformer = DeleteRow Int | AddRow [Int] | Sort
    deriving (Show)


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


agt :: Int -> DB -> Bool
agt th (DB rows) = all gr rows
    where
    gr row = all (> th) row
