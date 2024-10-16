-- Guards
doer :: Int -> Int -> Int
doer x y
    | z > y   = 1
    | z == y  = 0
    | z < y   = -1
  where z = x * x

ltgt :: Int -> String
ltgt x
    | x < 0     = "Negative"
    | x > 0     = "Positive"
    | otherwise = "Zero"

-- If-Else
ifefe :: Int -> Int -> Int
ifefe x y =
    let z = x * x
    in if z > y then
        1
       else if z == y then
        0
       else
        -1

-- Case
caser :: Int -> String
caser x = case x of
             1 -> "A"
             2 -> "B"
             3 -> "C"
             _ -> "Z"

-- Base Pattern Match
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

-- Factorial
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n *  factorial (n - 1)

-- Add Vectors
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors2 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Tuple Functions
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- Head Function
head' :: [a] -> a
head' [] = error "Can't call head on an empty list!"
head' (x:_) = x

-- Tell Function
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- Length Function
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Capital Function
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Density Functions
densityTell :: (RealFloat a) => a -> String
densityTell density
    | density < 1.2 = "Wow! You're going for a ride in the sky!"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."

densityTell2 :: (RealFloat a) => a -> a -> String
densityTell2 mass volume
    | mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"
    | mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."

densityTell3 :: (RealFloat a) => a -> a -> String
densityTell3 mass volume
    | density < 1.2 = "Wow! You're going for a ride in the sky!"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise = "If it's sink or swim, you're going to sink."
    where density = mass / volume

densityTell4 :: (RealFloat a) => a -> a -> String
densityTell4 mass volume
    | density < air = "Wow! You're going for a ride in the sky!"
    | density <= water = "Have fun swimming, but watch out for sharks!"
    | otherwise = "If it's sink or swim, you're going to sink."
    where
        density = mass / volume
        air = 1.2
        water = 1000.0

-- Initials Function
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where
        (f:_) = firstname
        (l:_) = lastname

-- Calculate Densities
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
    where density mass volume = mass / volume

-- Cylinder Function
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

-- Describe List
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."

-- Example Function
example :: Int -> String
example x
    | x < 0     = "Negative"
    | x > 0     = "Positive"
    | otherwise = "Zero"

-- Dictionary Data Type
data Dict = Mt | Entry String String Dict
    deriving (Show)

-- Find Function
find :: String -> Dict -> String
find _ Mt = ""
find target_key (Entry key value point)
    | target_key == key = value
    | otherwise = find target_key point

-- Combine Function
combine :: Dict -> Dict -> Dict
combine Mt d2 = d2
combine (Entry key value point) d2 =
    let combinedRest = combine point d2
    in if find key d2 == "" then Entry key (if value == "" then find key d2 else value) combinedRest else combinedRest

-- Main function to test all the functions
main :: IO ()
main = do
    -- Testing doer
    putStrLn "Testing doer 3 5:"
    print $ doer 3 5                      -- Should return -1

    -- Testing ltgt
    putStrLn "Testing ltgt -5:"
    print $ ltgt (-5)                     -- Should return "Negative"

    -- Testing ifefe
    putStrLn "Testing ifefe 3 4:"
    print $ ifefe 3 4                     -- Should return 1

    -- Testing caser
    putStrLn "Testing caser 2:"
    print $ caser 2                       -- Should return "B"

    -- Testing sayMe
    putStrLn "Testing sayMe 5:"
    print $ sayMe 5                       -- Should return "Five!"

    -- Testing factorial
    putStrLn "Testing factorial 5:"
    print $ factorial 5                   -- Should return 120

    -- Testing addVectors
    putStrLn "Testing addVectors (1, 2) (3, 4):"
    print $ addVectors (1, 2) (3, 4)      -- Should return (4, 6)

    -- Testing addVectors2
    putStrLn "Testing addVectors2 (1, 2) (3, 4):"
    print $ addVectors2 (1, 2) (3, 4)     -- Should return (4, 6)

    -- Testing first, second, third
    putStrLn "Testing first (1, 2, 3):"
    print $ first (1, 2, 3)               -- Should return 1
    putStrLn "Testing second (1, 2, 3):"
    print $ second (1, 2, 3)              -- Should return 2
    putStrLn "Testing third (1, 2, 3):"
    print $ third (1, 2, 3)                -- Should return 3

    -- Testing head'
    putStrLn "Testing head' [4, 5, 6]:"
    print $ head' [4, 5, 6]               -- Should return 4

    -- Testing tell
    putStrLn "Testing tell [1, 2]:"
    print $ tell [1, 2]                   -- Should return "This list is long. The first two elements are: 1 and 2"

    -- Testing length'
    putStrLn "Testing length' [1, 2, 3, 4]:"
    print $ length' [1, 2, 3, 4]          -- Should return 4

    -- Testing capital
    putStrLn "Testing capital \"Dracula\":"
    print $ capital "Dracula"              -- Should return "The first letter of Dracula is D"

    -- Testing densityTell
    putStrLn "Testing densityTell 400:"
    print $ densityTell 400                -- Should return "Have fun swimming, but watch out for sharks!"

    -- Testing densityTell2
    putStrLn "Testing densityTell2 400 1:"
    print $ densityTell2 400 1             -- Should return "Wow! You're going for a ride in the sky!"

    -- Testing densityTell3
    putStrLn "Testing densityTell3 5 2:"
    print $ densityTell3 5 2               -- Should return "Wow! You're going for a ride in the sky!"

    -- Testing densityTell4
    putStrLn "Testing densityTell4 5 2:"
    print $ densityTell4 5 2               -- Should return "Wow! You're going for a ride in the sky!"

    -- Testing initials
    putStrLn "Testing initials \"John\" \"Doe\":"
    print $ initials "John" "Doe"          -- Should return "J. D."

    -- Testing calcDensities
    putStrLn "Testing calcDensities [(100, 20), (50, 10), (75, 15)]:"
    print $ calcDensities [(100, 20), (50, 10), (75, 15)] -- Should return [5.0, 5.0, 5.0]

    -- Testing cylinder
    putStrLn "Testing cylinder 1 2:"
    print $ cylinder 1 2                   -- Should return ~18.84956

    -- Testing describeList
    putStrLn "Testing describeList [1, 2, 3]:"
    print $ describeList [1, 2, 3]         -- Should return "The list is a longer list."

    -- Testing example
    putStrLn "Testing example (-5):"
    print $ example (-5)                    -- Should return "Negative"

    -- Dictionary Example
    let eg = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
    putStrLn "Testing find \"Baz\" in dictionary:"
    print $ find "Baz" eg                  -- Should return "Ola"

    let d1 = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
    let d2 = Entry "Bingo1" "Bongo1" (Entry "Baz" "Ola" (Entry "Big1" "Deal1" Mt))
    putStrLn "Testing combine d1 and d2:"
    print $ combine d1 d2                   -- Combine dictionaries and print result
