
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