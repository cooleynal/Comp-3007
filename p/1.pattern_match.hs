
    -- guards
-- doer 3 5
doer :: Int -> Int -> Int
doer x y
        | z > y   =  1
        | z == y  =  0
        | z < y   =  -1
      where z = x*x


-- ltgt (-5)
ltgt :: Int -> String
ltgt x
    | x < 0     = "Negative"
    | x > 0     = "Positive"
    | otherwise = "Zero"


    -- if else if else
-- ifefe 3 4
ifefe :: Int -> Int -> Int
ifefe x y =
    let
      z = x * x
    in  -- Calculate z
      if z > y
        then 1
      else if z == y
        then 0
      else
        -1

    -- case of
-- caser 2
caser :: Int -> String
caser x = case x of
             1 ->  "A"
             2 ->  "B"
             3 ->  "C"
             _ ->  "Z"



    -- base pattern match
-- sayMe 5
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"





-- clean up below











-- factorial 5
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)



-- addVectors (1, 2) (3, 4)
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- addVectors2 (1, 2) (3, 4)
addVectors2 :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)




-- first (1, 2, 3)
first :: (a, b, c) -> a
first (x, _, _) = x

-- second (1, 2, 3)
second :: (a, b, c) -> b
second (_, y, _) = y

-- third(1, 2, 3)
third :: (a, b, c) -> c
third (_, _, z) = z


-- f [1, 2, 3, 4]
f :: [a] -> Maybe a
f (x:_) = Just x
f []    = Nothing  -- Return Nothing if the list is empty

s :: [a] -> Maybe a
s (_:y:_) = Just y
s _       = Nothing  -- Return Nothing if the list has less than two elements

t :: [a] -> Maybe a
t (_:_:z:_) = Just z
t _         = Nothing  -- Return Nothing if the list has less than three elements


xs :: [(Int, Int)]
xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]


sumPairs :: [(Int, Int)] -> [Int]
sumPairs xs = [a + b | (a, b) <- xs]

-- head' "Hello"
-- head' [4,5,6]
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- tell [1,2]
-- tell [1,2,3,4]
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- length [1,2,3,4]
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


-- capital "Dracula"
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- densityTell 400
densityTell :: (RealFloat a) => a -> String
densityTell density
    | density < 1.2 = "Wow! You're going for a ride in the sky!"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."


-- densityTell2 400 1
densityTell2 :: (RealFloat a) => a -> a -> String
densityTell2 mass volume
    | mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"
    | mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."


-- max' 5 10
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

-- 3 `myCompare` 2
-- myCompare 3 2

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- myCompare :: (Ord a) => a -> a -> Ordering
-- a `myCompare` b
--     | a > b     = GT
--     | a == b    = EQ
--     | otherwise = LT


-- densityTell3 5 2
densityTell3 :: (RealFloat a) => a -> a -> String
densityTell3 mass volume
    | density < 1.2 = "Wow! You're going for a ride in the sky!"
    | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
    | otherwise   = "If it's sink or swim, you're going to sink."
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



-- initials "John" "Doe"
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where
    (f:_) = firstname
    (l:_) = lastname


-- calcDensities [(100, 20), (50, 10), (75, 15)]
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
    where density mass volume = mass / volume


-- cylinder 1 2
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

-- let boot x y z = x * y + z in boot 3 4 2

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."


-- example (-5)
example :: Int -> String
example x
    | x < 0     = "Negative"
    | x > 0     = "Positive"
    | otherwise = "Zero"



data Dict = Mt | Entry String String Dict
    deriving (Show)

eg = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
eg1 = Entry "Bingo1" "Bongo1" (Entry "Baz1" "Ola1" (Entry "Big1" "Deal1" Mt))


-- find "Baz" eg = "Ola"
-- -- find "Egaah" eg = ""
find :: String -> Dict -> String
find _ Mt = ""
find target_key (Entry key value point)
    | target_key == key = value
    | otherwise = (find target_key point)


d1 :: Dict
d1 = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
d2 :: Dict
d2 = Entry "Bingo1" "Bongo1" (Entry "Baz" "Ola" (Entry "Big1" "Deal1" Mt))

combine :: Dict -> Dict -> Dict
combine Mt d2 = d2
combine (Entry key value point) d2 =
    let combinedRest = combine point d2
    in if find key d2 == "" then Entry key (if value == "" then find key d2 else value) combinedRest else combinedRest
