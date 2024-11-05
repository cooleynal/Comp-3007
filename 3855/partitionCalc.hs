

multiplesOf :: Int -> Int -> [Int]
multiplesOf n p = takeWhile (<= n) [0, p ..]

sumer :: Int -> Int -> [Int] -> [[Int]]
sumer _ _ [] = []
sumer 0 n (arga:argr) = multiplesOf n arga : sumer 1 n argr
sumer 1 n (arga:argr) = tail (multiplesOf n arga) : sumer 1 n argr
sumer _ _ _ = [] -- clear warning

combineAll :: Int -> [[Int]] -> [Int]
combineAll _ [] = []
combineAll n (x:xs) = foldl (addSums n) x xs
  where
    addSums :: Int -> [Int] -> [Int] -> [Int]
    addSums maxVal acc currentList =
      acc ++ [a + b | a <- acc, b <- currentList, a + b <= maxVal]


-- all partitions of size 1 to n
totalPartitions :: Int -> Int
totalPartitions 0 = 1
totalPartitions 1 = 1
totalPartitions n = length (combineAll n (sumer 0 n [2..n]))

-- selected partitions
selectPartitions :: Int -> [Int] -> Int
selectPartitions n l = length (combineAll nn (sumer 0 nn llf))
  where
    (nn, ll) = reduceValues n l
    llf = filter (>= 2) ll


gcd' :: Int -> Int -> Int
gcd' x 0 = abs x
gcd' x y = gcd' y (x `mod` y)


gcdList :: [Int] -> Int
gcdList [] = 0
gcdList [x] = abs x
gcdList (x:xs) = gcd' (abs x) (gcdList xs)


reduceValues :: Int -> [Int] -> (Int, [Int])
reduceValues maxVal parts = (newMaxVal, newParts)
  where
    commonDivisor = gcdList parts
    newMaxVal = maxVal `div` commonDivisor
    newParts = map (`div` commonDivisor) parts


main :: IO ()
main = do

  let size = 5
  let parts = [1,2, 4, 5]

  -- let loop n = do
  --       let partitions = totalPartitions n
  --       putStrLn $ "Total partitions of " ++ show n ++ ": " ++ show partitions
  --       loop (n + 1)
  -- loop 0



  print $ selectPartitions size parts
  -- print $ totalPartitions 22

  -- print $ additionsOf 20 4 10
