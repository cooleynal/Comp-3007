

-- getHead [5,4,3,2,1]
-- 5
getHead :: [a] -> a
getHead lst = head lst

-- getInit [5,4,3,2,1]
-- [5,4,3,2]
getInit :: [a] -> [a]
getInit lst = init lst


-- getTail [5,4,3,2,1]
-- [4,3,2,1]
getTail :: [a] -> [a]
getTail lst = tail lst

-- getLast [5,4,3,2,1]
-- 1
getLast :: [a] -> a
getLast lst = last lst

-- getLength [5,4,3,2,1]
-- 5
getLength :: [a] -> Int
getLength list = length list

-- getReverse [5,4,3,2,1]
-- [1,2,3,4,5]
getReverse :: [a] -> [a]
getReverse list = reverse list



-- Doubles a number
doubleMe :: Num a => a -> a
doubleMe x = x + x

-- Doubles two numbers and sums them
doubleUs :: Num a => a -> a -> a
doubleUs x y = x * 2 + y * 2

-- Doubles a number if it's 100 or less
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
                      then x
                      else x * 2

-- Gets the nth character from a string
getLetter :: String -> Int -> Char
getLetter "" _ = 'z'
getLetter str num = str !! num

-- Combines two lists of integers
combineWords :: [String] -> [String] -> [String]
combineWords adjectives nouns = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

-- Filters even numbers from a list of lists
filterEvens :: [[Int]] -> [[Int]]
filterEvens xxs = [[x | x <- xs, even x] | xs <- xxs]

-- Zips two lists together
ziper :: [a] -> [a] -> [(a, a)]
ziper a b = zip a b

-- Extracts the first elements from a list of pairs
fsts :: [(a, a)] -> [a]
fsts pairs = [fst pair | pair <- pairs]

-- Returns specific elements from a list based on conditions
xer :: Int -> [Int]
xer a | a < 0     = []
       | a == 0    = []
       | otherwise  = [y | y <- [1..a], (even y && y > 5) || y == 1]

-- Main function to test each function
-- Main function to test each function
main :: IO ()
main = do
    -- Testing doubleMe
    putStrLn "Testing doubleMe with input 5:"
    print $ doubleMe 5                 -- Should return 10

    -- Testing doubleUs
    putStrLn "Testing doubleUs with inputs 3 and 4:"
    print $ doubleUs 3 4               -- Should return 14

    -- Testing doubleSmallNumber
    putStrLn "Testing doubleSmallNumber with input 50:"
    print $ doubleSmallNumber 50       -- Should return 100
    putStrLn "Testing doubleSmallNumber with input 150:"
    print $ doubleSmallNumber 150      -- Should return 150

    -- Testing getLetter
    putStrLn "Testing getLetter with string 'hey guys' and index 4:"
    print $ getLetter "hey guys" 4     -- Should return 'g'
    putStrLn "Testing getLetter with empty string:"
    print $ getLetter "hello" 0        -- Should return 'h'

    -- Testing combineWords
    let nouns = ["hobo", "frog", "pope"]
    let adjectives = ["lazy", "grouchy", "scheming"]
    putStrLn "Testing combineWords with adjectives and nouns:"
    print $ combineWords adjectives nouns -- Should combine the words

    -- Testing filterEvens
    let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
    putStrLn "Testing filterEvens with a list of lists:"
    print $ filterEvens xxs             -- Should return lists of even numbers

    -- Testing ziper
    putStrLn "Testing ziper with two lists:"
    print $ ziper [1,2,3,4,5] [5,5,5,5,5]  -- Should return pairs of zipped lists

    -- Testing fsts
    let pairs = ziper [1,2,3] [4,5,6]
    putStrLn "Testing fsts with pairs:"
    print $ fsts pairs                    -- Should return [1,2,3]

    -- Testing xer
    putStrLn "Testing xer with input 10:"
    print $ xer 10                        -- Should return [1, 6, 8, 10]
    putStrLn "Testing xer with input 0:"
    print $ xer 0                         -- Should return []
    putStrLn "Testing xer with negative input:"
    print $ xer (-5)                     -- Should return []

    -- Additional testing of empty inputs
    putStrLn "Testing getLetter with an empty string:"
    print $ getLetter "" 0                -- Should return 'z'
    putStrLn "Testing fsts with an empty list:"
    print $ fsts ([] :: [(Int, Int)])    -- Specify type as a list of pairs of Ints
