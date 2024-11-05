-- Zero type class definition
class Zero a where
    zero :: a

-- Instance of Zero for Int
instance Zero Int where
    zero = 0

-- Instance of Zero for lists
instance Zero [a] where
    zero = []

-- Instance of Zero for tuples (a, b)
instance (Zero a, Zero b) => Zero (a, b) where
    zero = (zero, zero)

-- Instance of Zero for Bool
instance Zero Bool where
    zero = False

-- Instance of Zero for Maybe
instance Zero (Maybe a) where
    zero = Nothing

-- Lookup function using Zero type class
myLookup :: (Eq a, Zero b) => a -> [(a, b)] -> b
myLookup x l = case lookup x l of
    Just val -> val
    Nothing  -> zero

-- Function using zero to check conditions
foo :: Int -> Bool
foo n =
    if n == zero
    then zero
    else null (n : zero)

-- Functor type class definition (renamed to CustomFunctor)
class CustomFunctor f where
    customFmap :: (a -> b) -> f a -> f b

-- Instance of CustomFunctor for lists
instance CustomFunctor [] where
    customFmap f l = map f l

-- Instance of CustomFunctor for Maybe
instance CustomFunctor Maybe where
    customFmap f (Just x)  = Just (f x)
    customFmap f Nothing   = Nothing

-- Tree data type
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Instance of Show for Tree
instance Show a => Show (Tree a) where
    show (Leaf x)     = "Leaf " ++ show x
    show (Node t0 t1) = "Node (" ++ show t0 ++ ") (" ++ show t1 ++ ")"

-- Instance of CustomFunctor for Tree
instance CustomFunctor Tree where
    customFmap f (Leaf x)     = Leaf (f x)
    customFmap f (Node t0 t1) = Node (customFmap f t0) (customFmap f t1)

-- Functor instance for tuples with a fixed first element (renamed)
-- customFmap (+1) (10, 20)
instance CustomFunctor ((,) r) where
    customFmap f (x, y) = (x, f y)

-- Functor instance for functions (renamed)
instance CustomFunctor ((->) r) where
    customFmap f g = \x -> f (g x)  -- This is equivalent to f . g

-- Monad type class definition (renamed to CustomMonad)
class CustomMonad m where
    customBind :: m a -> (a -> m b) -> m b  -- The "bind" operator
    customThen :: m a -> m b -> m b         -- Special case of the bind operator
    customReturn :: a -> m a                -- Insert a value

-- Instance of CustomMonad for Maybe
instance CustomMonad Maybe where
    Just x `customBind` f = f x
    Nothing  `customBind` _ = Nothing
    customReturn x = Just x
    a `customThen` b = a `customBind` \_ -> b  -- Implementing customThen

-- Instance of CustomMonad for IO
instance CustomMonad IO where
    a `customBind` f = do
        x <- a
        f x
    customThen a b = a `customBind` \_ -> b  -- Implementing customThen
    customReturn = Prelude.return  -- Use Prelude's return to avoid ambiguity

-- Instance of CustomMonad for lists
instance CustomMonad [] where
    l `customBind` f = concat (map f l)
    customReturn x = [x]
    a `customThen` b = a `customBind` \_ -> b  -- Implementing customThen

-- Example usage of do-notation with lists
example :: [(Int, Int)]
example = do
    x <- [1, 2, 3]
    y <- [4, 5, 6]
    customReturn (x, y)

-- Functor examples
functorExamples :: IO ()
functorExamples = do
    -- Example with lists
    let listExample = customFmap (*2) [1, 2, 3]  -- Expect [2, 4, 6]
    putStrLn $ "List example (customFmap): " ++ show listExample

    -- Example with Maybe
    let maybeExample1 = customFmap (+1) (Just 5)  -- Expect Just 6
    putStrLn $ "Maybe example (Just 5): " ++ show maybeExample1

    let maybeExample2 = customFmap (+1) Nothing   -- Expect Nothing
    putStrLn $ "Maybe example (Nothing): " ++ show maybeExample2

    -- Example with Tree
    let treeExample = customFmap (*10) (Node (Leaf 1) (Leaf 2))  -- Expect Node (Leaf 10) (Leaf 20)
    putStrLn $ "Tree example (customFmap): " ++ show treeExample

-- Main function to demonstrate usage
main :: IO ()
main = do
    putStrLn "Demonstrating myLookup:"
    let lookupResult = myLookup 1 [(1, "one"), (2, "two"), (3, "three")]
    print lookupResult

    putStrLn "Demonstrating example with lists:"
    print example

    putStrLn "Using foo function:"
    print $ foo 0  -- Should return False
    print $ foo 1  -- Should return True

    -- Call the functor examples
    functorExamples
