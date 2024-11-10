
-- Eq	            Equality comparison             	    ==, /=
-- Ord	            Ordering	                            <, >, compare
-- Show	            Convert to string	                    show
-- Read	            Parse from string	                    read
-- Num	            Numeric operations	                    +, -, *
-- Fractional	    Fractional operations	                /, recip
-- Integral	        Integral numeric types	                div, mod
-- Functor          Map over values	                        fmap
-- Applicative	    Apply functions wrapped in a context	pure, <*>
-- Monad	        Use in a monadic context	            >>=, return
-- Traversable	    Traverse data structures	            traverse



import qualified Data.Maybe as M

-- Eq
-- Equality comparison
-- Operators: ==, /=
eqFunction :: Eq a => a -> a -> Bool
eqFunction a b = a == b

exampleEq :: Bool
exampleEq = eqFunction 5 5  -- True

-- Ord
-- Ordering
-- Operators: <, >, compare
ordFunction :: Ord a => a -> a -> Ordering
ordFunction a b = compare a a


exampleOrd :: Ordering
exampleOrd = ordFunction 10 5  -- GT

-- Show
-- Convert to string
-- Function: show
showFunction :: Show a => a -> String
showFunction x = show x

exampleShow :: String
exampleShow = showFunction 12345  -- "12345"

-- Read
-- Parse from string
-- Function: read
readFunction :: Read a => String -> a
readFunction s = read s

exampleRead :: Int
exampleRead = readFunction "12345"  -- 12345

-- Num
-- Numeric operations
-- Operators: +, -, *
numFunction :: Num a => a -> a -> a
numFunction a b = a + b

exampleNum :: Int
exampleNum = numFunction 5 20  -- 25

-- Fractional
-- Fractional operations
-- Operators: /, recip
fractionalFunction :: Fractional a => a -> a -> a
fractionalFunction a b = a / b

exampleFractional :: Float
exampleFractional = fractionalFunction 5.0 2.0  -- 2.5

-- Integral
-- Integral numeric types
-- Operators: div, mod
integralFunction :: Integral a => a -> a -> a
integralFunction a b = div a b

exampleIntegral :: Integer
exampleIntegral = integralFunction 10 3  -- 3

-- Functor
-- Map over values
-- Function: fmap
functorFunction :: Functor f => (a -> b) -> f a -> f b
functorFunction f x = fmap f x

exampleFunctor :: Maybe Int
exampleFunctor = functorFunction (+1) (Just 5)  -- Just 6

-- Applicative
-- Apply functions wrapped in a context
-- Functions: pure, <*>
applicativeFunction :: Applicative f => f (a -> b) -> f a -> f b
applicativeFunction f x = f <*> x

exampleApplicative :: Maybe Int
exampleApplicative = applicativeFunction (Just (+1)) (Just 5)  -- Just 6

-- Monad
-- Use in a monadic context
-- Functions: >>=, return
monadFunction :: Monad m => m a -> (a -> m b) -> m b
monadFunction m f = m >>= f

exampleMonad :: Maybe Int
exampleMonad = monadFunction (Just 5) (\x -> Just (x + 1))  -- Just 6

-- Traversable
-- Traverse data structures
-- Function: traverse
traverseFunction :: (Traversable t) => (a -> b) -> t a -> t b
traverseFunction f xs = fmap f xs  -- Using fmap directly for lists

exampleTraversable :: [Int]
exampleTraversable = traverseFunction (*2) [1, 2, 3]  -- [2, 4, 6]







-- Data definitions
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Surface area calculation
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Example shapes
cs :: Shape
cs = Circle (Point 10 20) 30

rs :: Shape
rs = Rectangle (Point 11 22) (Point 33 44)

-- Person data type using record syntax
data Person1 = Person1
    { firstName1 :: String
    , lastName1 :: String
    , age1 :: Int
    , height1 :: Float
    , phoneNumber1 :: String
    , flavor1 :: String
    } deriving (Show)

-- Example Person
ps1 :: Person1
ps1 = Person1 "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- Car data type
data Car a b c = Car
    { company :: a
    , model :: b
    , year :: c
    } deriving (Show)

tellCar :: (Show c) => Car String String c -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- Vector data type
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
scalarMult (Vector i j k) (Vector l m n) = i*l + j*m + k*n

-- Main function to execute examples
main :: IO ()
main = do
    -- Eq examples
    putStrLn "Testing Eq:"
    putStrLn $ "eqFunction 5 5: " ++ show exampleEq  -- True
    putStrLn ""

    -- Ord examples
    putStrLn "Testing Ord:"
    putStrLn $ "ordFunction 10 5: " ++ show exampleOrd  -- GT
    putStrLn ""

    -- Show examples
    putStrLn "Testing Show:"
    putStrLn $ "showFunction 12345: " ++ exampleShow  -- "12345"
    putStrLn ""

    -- Read examples
    putStrLn "Testing Read:"
    putStrLn $ "readFunction \"12345\": " ++ show exampleRead  -- 12345
    putStrLn ""

    -- Num examples
    putStrLn "Testing Num:"
    putStrLn $ "numFunction 5 20: " ++ show exampleNum  -- 25
    putStrLn ""

    -- Fractional examples
    putStrLn "Testing Fractional:"
    putStrLn $ "fractionalFunction 5.0 2.0: " ++ show exampleFractional  -- 2.5
    putStrLn ""

    -- Integral examples
    putStrLn "Testing Integral:"
    putStrLn $ "integralFunction 10 3: " ++ show exampleIntegral  -- 3
    putStrLn ""

    -- Functor examples
    putStrLn "Testing Functor:"
    putStrLn $ "functorFunction (+1) (Just 5): " ++ show exampleFunctor  -- Just 6
    putStrLn ""

    -- Applicative examples
    putStrLn "Testing Applicative:"
    putStrLn $ "applicativeFunction (Just (+1)) (Just 5): " ++ show exampleApplicative  -- Just 6
    putStrLn ""

    -- Monad examples
    putStrLn "Testing Monad:"
    putStrLn $ "monadFunction (Just 5) (\\x -> Just (x + 1)): " ++ show exampleMonad  -- Just 6
    putStrLn ""

    -- Traversable examples
    putStrLn "Testing Traversable:"
    putStrLn $ "traverseFunction (*2) [1, 2, 3]: " ++ show exampleTraversable  -- [2, 4, 6]

    -- Shape examples
    putStrLn "Testing Shape Surface Area:"
    putStrLn $ "Surface of Circle: " ++ show (surface cs)  -- Surface of Circle
    putStrLn $ "Surface of Rectangle: " ++ show (surface rs)  -- Surface of Rectangle

    -- Person example
    putStrLn "Person Example:"
    putStrLn $ show ps1  -- Show person example

    -- Car example
    let myCar = Car "Toyota" "Camry" 2020
    putStrLn $ tellCar myCar  -- Tell car details

    -- Vector examples
    let v1 = Vector 1 2 3
    let v2 = Vector 4 5 6
    putStrLn $ "Vector addition: " ++ show (vplus v1 v2)  -- Vector addition
    putStrLn $ "Vector scalar multiplication: " ++ show (vectMult v1 3)  -- Vector scalar multiplication
    putStrLn $ "Vector scalar product: " ++ show (scalarMult v1 v2)  -- Vector scalar product
