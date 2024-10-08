
-- normal recursion
-- fact 5
fact :: Int -> Int
fact 0 = 1
fact a = a * fact (a - 1)

-- tail recursion
-- factTail 5
factTail :: Int -> Int
factTail a = helper a 1
  where
    helper 0 acc = acc
    helper a acc = helper (a-1) (a * acc)



stirling :: Int -> Int -> Int
stirling n 1 = 1
stirling 0 0 = 1
stirling n 0 = 0
stirling n k
  | n < k     = 0
  | otherwise = stirling (n - 1) (k - 1) + k * stirling (n - 1) k

-- normal recursion
-- fib 33
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- tail recursion
-- fibTail 33
fibTail :: Int -> Int
fibTail a = fibTail' a (0, 1)

fibTail' :: Int -> (Int, Int) -> Int
fibTail' 0 (a, _) = a
fibTail' 1 (_, b) = b
fibTail' n (a, b) = fibTail' (n - 1) (b, a + b)


main :: IO ()
main = do
  putStrLn $ "Factof 5: " ++ show (fact 5)
  putStrLn $ "FactTail of 5: " ++ show (factTail 5)

  putStrLn $ "Fibonacci of 5: " ++ show (fib 5)
  putStrLn $ "Fibonacci of 5: " ++ show (fibTail 5)

  putStrLn $ "stirling of n=7 k=3: " ++ show (stirling 7 3)