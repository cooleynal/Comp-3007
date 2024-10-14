-- module Main where

-- import Prelude hiding (tail)

-- Define list examples
list1 :: [Int]
list1 = 5 : 3 : 17 : []

list2 :: [Int]
list2 = 5 : (3 : (17 : []))

list3 :: [Int]
list3 = [5, 3, 17]

-- Check if all lists are equal
checkLists :: Bool
checkLists = list1 == list2 && list1 == list3 && list2 == list3

-- Define custom tail function
myTail :: [a] -> [a]
myTail [] = []
myTail (_ : l) = l

-- Function to ignore the second element of a list
ignoreSecond :: [a] -> [a]
ignoreSecond [] = []             -- Empty list
ignoreSecond (x : _ : xs) = x : xs

-- Define function to concatenate a list of Ints into a String
concatInts :: [Int] -> String
concatInts = concatMap show

numbers :: [Int]
numbers = [1, 2, 3, 42]

-- Define `eg` using the `concatInts` function
eg :: String
eg = concatInts numbers ++ " and more text"

-- Define `myAll` using map and filter
myAll :: (a -> Bool) -> [a] -> Bool
myAll p l = null (filter (not . p) l)

-- Function to add one to each element in a list
addOne :: [Int] -> [Int]
addOne [] = []
addOne (x : xs) = (x + 1) : (addOne xs)

-- Function to count elements in a list
countElements :: [a] -> Int
countElements = length

-- Function to remove duplicates from a list
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = removeHelper []
  where
    removeHelper :: (Eq a) => [a] -> [a] -> [a]
    removeHelper seen [] = reverse seen
    removeHelper seen (x:xs)
      | x `elem` seen = removeHelper seen xs
      | otherwise     = removeHelper (x : seen) xs

-- Function to check if the sum of two numbers is greater than 10
f :: (Num a, Ord a) => a -> a -> Bool
f x y = x + y > 10

-- Function to double a number
double :: Num a => a -> a
double x = x * 2

-- Function to append an element to a list
append :: [a] -> a -> [a]
append [] y = [y]
append (x : l) y = x : append l y

-- Function to listify a list (convert to a list of lists)
listize :: [a] -> [[a]]
listize [] = []
listize (x : l) = [x] : listize l

-- Custom addition operation
(+++) :: Int -> Int -> Int
x +++ y = x + 2 * y

-- Function to generate odd numbers from a given number
f2 :: Int -> Int
f2 x = 2 * x + 1

-- Define the binary tree type
data BT a = Leaf a | Node (BT a) (BT a)

-- Implement Show for BT
instance (Show a) => Show (BT a) where
    show (Leaf x) = "Leaf " ++ show x
    show (Node l r) = "Node (" ++ show l ++ ") (" ++ show r ++ ")"

-- Function to get the left subtree
left :: BT a -> BT a
left (Node l _) = l
left (Leaf _) = error "Leaf does not have a left subtree"

-- Function to get the right subtree
right :: BT a -> BT a
right (Node _ r) = r
right (Leaf _) = error "Leaf does not have a right subtree"

-- Follow left branches three times, assuming it's possible
lll :: BT a -> BT a
lll t = left $ left $ left t

-- Example binary tree
exampleTree :: BT Int
exampleTree = Node
                (Node
                    (Node (Leaf 1) (Leaf 2))
                    (Node (Leaf 3) (Leaf 4))
                )
                (Node
                    (Leaf 5)
                    (Node (Leaf 6) (Leaf 7))
                )

-- Function to compute the absolute value of an integer
absVal :: Int -> Int
absVal n | n < 0     = -n
         | otherwise = n

-- Function to insert into a sorted list
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : l) | x <= y = x : y : l
insert x (y : l) = y : (insert x l)

-- Main function to test all functions
main :: IO ()
main = do
    -- Test list equality
    putStrLn $ "Are all lists equal? " ++ show checkLists

    -- Test custom tail function
    putStrLn $ "Tail of list1: " ++ show (myTail list1)

    -- Test ignoreSecond function
    putStrLn $ "Ignore second of [1, 2, 3, 4]: " ++ show (ignoreSecond [1, 2, 3, 4])

    -- Test concatInts function
    putStrLn $ "Concatenated integers: " ++ concatInts numbers

    -- Test myAll function
    putStrLn $ "Are all elements in [2, 4, 6] even? " ++ show (myAll even [2, 4, 6])

    -- Test addOne function
    putStrLn $ "Add one to [1, 2, 3]: " ++ show (addOne [1, 2, 3])

    -- Test countElements function
    putStrLn $ "Count of elements in [1, 2, 3]: " ++ show (countElements [1, 2, 3])

    -- Test removeDuplicates function
    putStrLn $ "Remove duplicates from [1, 2, 3, 2, 4, 1]: " ++ show (removeDuplicates [1, 2, 3, 2, 4, 1])

    -- Test function f
    putStrLn $ "Does 5 + 7 > 10? " ++ show (f 5 7)

    -- Test double function
    putStrLn $ "Double of 4: " ++ show (double 4)

    -- Test append function
    putStrLn $ "Append 5 to [1, 2, 3]: " ++ show (append [1, 2, 3] 5)

    -- Test listize function
    putStrLn $ "Listize [1, 2, 3]: " ++ show (listize [1, 2, 3])

    -- Test custom addition (+++) function
    putStrLn $ "Custom addition 3 +++ 2: " ++ show (3 +++ 2)

    -- Test f2 function
    putStrLn $ "Result of f2(5): " ++ show (f2 5)

    -- Test binary tree
    putStrLn $ "Example tree: " ++ show exampleTree
    putStrLn $ "Left subtree of example tree: " ++ show (left exampleTree)
    putStrLn $ "Right subtree of example tree: " ++ show (right exampleTree)
    putStrLn $ "Follow left three times: " ++ show (lll exampleTree)

    -- Test absolute value function
    putStrLn $ "Absolute value of -10: " ++ show (absVal (-10))

    -- Test insert function
    putStrLn $ "Insert 3 into [1, 2, 4]: " ++ show (insert 3 [1, 2, 4])
