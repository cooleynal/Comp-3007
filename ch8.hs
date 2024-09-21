





-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
--     deriving (Show)



data Point = Point Float Float
    deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

-- Circle 10 20 30
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


cs :: Shape
cs = Circle (Point 10 20) 30

rs :: Shape
rs = Rectangle (Point 11 22) (Point 33 44)

-- ghci> map (Circle 10 20) [4,5,6,6]
-- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]



-- data Person = Person String String Int Float String String
--     deriving (Show)


-- ps :: Person
-- ps = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"


-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname

-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor




ps1 :: Person1
ps1 = Person1 "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

data Person1 = Person1 { firstName1 :: String
                     , lastName1 :: String
                     , age1 :: Int
                     , height1 :: Float
                     , phoneNumber1 :: String
                     , flavor1 :: String
                     } deriving (Show)


-- data Car = Car String String Int deriving (Show)

-- c1 :: Car
-- c1 = Car "Ford" "Must" 1967

-- data Car = Car {
--     company :: String,
--     model :: String,
--     year :: Int
-- } deriving (Show)



-- tellCar :: Car -> String
-- tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


data Car a b c = Car { company :: a
, model :: b
, year :: c
} deriving (Show)

tellCar :: (Show c) => Car String String c -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y



data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
-- (Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
-- (Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
vectMult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
-- (Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
scalarMult (Vector i j k) (Vector l m n) = i*l + j*m + k*n


data Person = Person { firstName :: String
, lastName :: String
, age :: Int
} deriving (Eq, Show)



mikeD :: Person
mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}

adRock :: Person
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}

-- mikeD == mikeD1
mikeD1 :: Person
mikeD1 = Person {firstName = "Michael1", lastName = "Diamond", age = 43}

-- Just 3 `compare` Just 2
-- compare (Just 3) (Just 2)


type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook


data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]

-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
