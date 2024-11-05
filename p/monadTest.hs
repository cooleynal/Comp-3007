


-- Naive implementation for computations that might fail
safeAddNaive :: Maybe Int -> Maybe Int -> Maybe Int
safeAddNaive (Just x) (Just y) = Just (x + y)  -- If both are Just, add them
safeAddNaive _ _ = Nothing                          -- If any are Nothing, return Nothing




-- Monad implementation for computations that might fail
safeAddMonad :: Maybe Int -> Maybe Int -> Maybe Int
safeAddMonad mx my = do
    x <- mx                               -- Extract value from mx
    y <- my                               -- Extract value from my
    return' (x + y)                       -- Return the sum wrapped in Just

-- Example values
getValue1 :: Maybe Int
getValue1 = Just 5

getValue2 :: Maybe Int
getValue2 = Just 10




------------------------------------------------





-- Define the custom Monad' class
class Monad' m where
    -- >>=
    (>>=!) :: m a -> (a -> m b) -> m b  -- The "bind" operator
    -- >>
    (>>!) :: m a -> m b -> m b           -- Just a special case of the bind operator
    return' :: a -> m a                   -- Insert a value

-- Instance for Maybe
instance Monad' Maybe where
    Just x >>=! f = f x                   -- If Just, apply the function f
    Nothing >>=! _ = Nothing               -- If Nothing, return Nothing
    return' x = Just x                     -- Wrap a value in Just

    Just _ >>! mb = mb                      -- If we have Just a, just return mb
    Nothing >>! _ = Nothing                 -- If we have Nothing, return Nothing


instance Monad' IO where
  action >>=! f = action >>= f
  action >>! nextAction = action >> nextAction
  return' = return


-- equivalent
-- Calculate sum using the do notation
calculateSumDo :: Maybe Int
calculateSumDo = do
    x1 <- getValue1      -- Get first value
    x2 <- getValue2      -- Get second value
    return' (x1 + x2)    -- Return the sum wrapped in Just

-- Calculate sum using the bind operator
calculateSumBind :: Maybe Int
calculateSumBind =
    getValue1 >>=! \x1 ->  -- Get first value
    getValue2 >>=! \x2 ->  -- Get second value
    return' (x1 + x2)      -- Return the sum wrapped in Just




-- pOne now returns a Maybe Int
pOne :: Int -> Maybe Int
pOne n = Just (n + 1)  -- Wrap the result in a Just

-- calc uses do-notation with Maybe
calc :: Maybe Int -> Maybe Int
calc (Just n) = do
    x1 <- pOne n  -- pOne 1 returns Just 2
    x2 <- pOne n  -- pOne 2 returns Just 3
    return' (x1 + x2)  -- Return the sum wrapped in Just
calc Nothing = Nothing  -- If the input is Nothing, return Nothing


------------------------------------------------

-- Define a simple function to demonstrate
increment :: Int -> Int
increment x = x + 1

double :: Int -> Int
double x = x * 2

printResult :: Int -> IO ()
printResult x = putStrLn $ "Result: " ++ show x

-- Using >> operator
doOperators :: IO ()
doOperators =
    printResult 1 >>!          -- Print Result: 1
    printResult (increment 1) >>! -- Print Result: 2
    printResult (double 2) >>!     -- Print Result: 4
    printResult (increment 3)      -- Print Result: 4

-- Using do notation
doNotation :: IO ()
doNotation = do
    printResult 1               -- Print Result: 1
    printResult (increment 1)   -- Print Result: 2
    printResult (double 2)      -- Print Result: 4
    printResult (increment 3)   -- Print Result: 4



-- Main function to demonstrate the comparison
main :: IO ()
main = do
    let a = Just 5
    let b = Just 10
    let c = Nothing

    print calculateSumDo    -- Should print: Just 15
    print calculateSumBind   -- Should print: Just 15

    ------------------------------------------------

    putStrLn "Using Naive Approach:"
    print (safeAddNaive a b) -- Just (15)
    print (safeAddNaive a c) -- Nothing
    print (safeAddNaive c c) -- Nothing


    putStrLn "\nUsing Monad':"
    print (safeAddMonad a b) -- Just (15)
    print (safeAddMonad a c) -- Nothing
    print (safeAddMonad c c) -- Nothing

    ------------------------------------------------

    putStrLn "\nUsing >> Operator:"
    doOperators
    putStrLn "\nUsing do Notation:"
    doNotation

    ------------------------------------------------

    putStrLn "\ncalc (Just 5):"
    print $ calc (Just 5)
