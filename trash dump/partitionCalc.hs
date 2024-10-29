







-- [0,2,4,6,8,10,12,14,16,18,20]
-- max exponent, power raise



-- pass in 1 index
-- additionsOf :: Int -> Int -> Int -> [Int]
-- additionsOf n a p     | a > p =  takeWhile (<= n) [a + p, a + 2 * p ..]
-- additionsOf n p a     =  takeWhile (<= n) [a + p, a + 2 * p ..]



multiplesOf :: Int -> Int -> [Int]
multiplesOf n p = takeWhile (<= n) [0, p ..]

sumer :: Int -> Int -> [Int] -> [[Int]]
sumer _ _ [] = []
sumer 0 n (arga:argr) = multiplesOf n arga : sumer 1 n argr
sumer 1 n (arga:argr) = tail (multiplesOf n arga) : sumer 1 n argr
sumer _ _ _ = [] -- clear warning



pairwiseSums :: Int -> [[Int]] -> [Int]
pairwiseSums maxLimit [xs, ys] = [x + y | x <- xs, y <- ys, x + y <= maxLimit]




main :: IO ()
main = do

  let maxVal = 20
  let parts = [4, 10]
  -- print $ multiplesOf 20 2
  -- print $ multiplesOf 20 5

  let result = sumer 0 maxVal parts
  print $ result

  let pr = pairwiseSums maxVal result
  print $ pr
  print $ length pr

  -- print $ additionsOf 20 4 10
