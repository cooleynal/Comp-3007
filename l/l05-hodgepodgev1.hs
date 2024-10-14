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

-- Define a dummy main function
main :: IO ()
main = return ()


addOne :: [Int] -> [Int]
addOne [] = []
addOne (x : xs) = (x + 1) : (addOne xs)


countElements :: [a] -> Int
countElements = length



-- removeDuplicates [1, 2, 3, 2, 4, 1]

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = removeHelper []
  where
    removeHelper :: (Eq a) => [a] -> [a] -> [a]
    removeHelper seen [] = reverse seen
    removeHelper seen (x:xs)
      | x `elem` seen = removeHelper seen xs
      | otherwise     = removeHelper (x : seen) xs


f :: (Num a, Ord a) => a -> a -> Bool
f x y = x + y > 10


double :: Num a => a -> a
double x = x * 2


-- [a, b, c, d]

append :: [a] -> a -> [a]
append [] y = [y]
append (x : l) y = x : append l y





listize :: [a] -> [[a]]
listize [] = []
listize (x : l) = [x] : listize l



-- :type (1 +++ 2)
(+++) :: Int -> Int -> Int
x +++ y = x + 2 * y


f2 :: Int -> Int
f2 x = 2 * x + 1


-- data BT a = Leaf a | Node (BT a) (BT a)

-- left(Node l _) = l
-- right(Node _ r) = r

-- -- Follow left branches three times, assuming it's possible
-- lll :: BT a -> BT a
-- lll t = left $ left $ left t



-- -- Example binary tree
-- exampleTree :: BT Int
-- exampleTree = Node
--                 (Node
--                     (Node (Leaf 1) (Leaf 2))
--                     (Node (Leaf 3) (Leaf 4))
--                 )
--                 (Node
--                     (Leaf 5)
--                     (Node (Leaf 6) (Leaf 7))
--                 )



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
lll t = left $ left $ left t -- l (l (l t))

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



-- absVal n | n < 0 = -n
-- absVal n = n

absVal :: Int -> Int
absVal n | n < 0     = -n
         | otherwise = n

-- insert into a sorted list
-- insert 3 [1,2,3,4] = [1,2,3,3,4]
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : l) | x <= y = x : y : l
insert x (y : l) = y : (insert x l)