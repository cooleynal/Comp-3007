







multiplesOf :: Int -> Int -> [Int]
multiplesOf n p = takeWhile (<= n) [0, p ..]

sumer :: Int -> Int -> [Int] -> [[Int]]
sumer _ _ [] = []
sumer 0 n (arga:argr) = multiplesOf n arga : sumer 1 n argr
sumer 1 n (arga:argr) = tail (multiplesOf n arga) : sumer 1 n argr
sumer _ _ _ = [] -- clear warning



pairwiseSums :: Int -> [[Int]] -> [Int]
pairwiseSums n lists = filter (<= n) $ combineAll lists

combineAll :: [[Int]] -> [Int]
combineAll [] = []
combineAll (x:xs) = foldl addSums x xs
  where
    addSums acc currentList = acc ++ [a + b | a <- acc, b <- currentList]



main :: IO ()
main = do

  let maxVal = 300
  -- let parts = [4, 3, 10]
  -- let parts = [4,  10]
  let parts = [5, 25, 10] -- exclude 1

  let result = sumer 0 maxVal parts
  print $ result

  let pr = pairwiseSums maxVal result
  print $ pr
  print $ length pr

  -- print $ additionsOf 20 4 10
