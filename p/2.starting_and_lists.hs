




doubleMe :: Num a => a -> a
doubleMe x = x + x


doubleUs :: Num a => a -> a -> a
doubleUs x y = x*2 + y*2



-- doubleSmallNumber 5
-- 10
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
                        then x
                        else x*2



-- getLetter "hey guys" 4
-- 'g'
getLetter :: String -> Int -> Char
getLetter "" _ = 'z'
getLetter str num = str !! num


-- [1,2,3,4] ++ [9,10,11,12]
-- [1,2,3,4,9,10,11,12]

-- 'A':" SMALL CAT"
-- "A SMALL CAT"

compareLists :: [Int] -> [Int] -> Bool
compareLists list1 list2 = list1 > list2

compareLists1 :: [Int] -> [Int] -> Bool
compareLists1 list1 list2 = head list1 > head list2



-- getHead [5,4,3,2,1]
-- 5
getHead :: [a] -> a
getHead lst = head lst


-- getTail [5,4,3,2,1]
-- [4,3,2,1]
getTail :: [a] -> [a]
getTail lst = tail lst

-- getLast [5,4,3,2,1]
-- 1
getLast :: [a] -> a
getLast lst = last lst


-- getInit [5,4,3,2,1]
-- [5,4,3,2]
getInit :: [a] -> [a]
getInit lst = init lst

-- getLength [5,4,3,2,1]
-- 5
getLength :: [a] -> Int
getLength list = length list

-- getReverse [5,4,3,2,1]
-- [1,2,3,4,5]
getReverse :: [a] -> [a]
getReverse list = reverse list

-- takeCycle 10 [1,2,3]

takeCycle :: Int -> [a] -> [a]
takeCycle n lst = take n (cycle lst)

-- [ x | x <- [50..100], x `mod` 7 == 3]
-- [52,59,66,73,80,87,94]


-- [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
-- [10,11,12,14,16,17,18,20]




-- ghci> nouns = ["hobo","frog","pope"]
-- ghci> adjectives = ["lazy","grouchy","scheming"]
-- ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
combineWords :: [String] -> [String] -> [String]
combineWords adjectives nouns = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

xxs :: [[Int]]
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

fe :: [[Int]] -> [[Int]]
fe xxs = [[x | x <- xs, even x] | xs <- xxs]

-- even 1
-- even 2
-- ziper [1,2,3,4,5] [5,5,5,5,5]
ziper :: [a] -> [a] -> [(a, a)]
ziper a b = zip a b

-- fsts (ziper [1,2,3,4,5] [5,5,5,5,5])
fsts :: [(a, a)] -> [a]
-- fsts _ = []
-- fsts a = fst a
fsts pairs = [fst pair | pair <- pairs]
-- fsts pairs = [fst pair |  pairs -> pair]

-- <- to iterate through elements in a list.
-- -> is used for function definitions or type annotations.

-- [result | condition1, condition2, ...]


xer :: Int -> [Int]
xer a   | a < 0 = []
        | a == 0   = []
        | otherwise  = let x = [1..a]
        in [y | y <- x, (even y && y > 5) || y == 1]
-- xer x = [x  | x <- [1..10], even x, x > 5
--             | x <- [1..10] x == 1]


xer1 :: Int -> [Int]
xer1 a  | a < 0 = []
        | a == 0   = []
        | otherwise  = [x | x <- [1..a], (even x && x > 5) || x == 1]


-- [y | y <- [1..10], y > 5, even y]
-- [6,8,10]