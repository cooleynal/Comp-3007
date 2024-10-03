
-- stirling 15 8
-- stirling 55 2
stirling :: Int -> Int -> Int
stirling n 1 = 1
stirling 0 0 = 1
stirling n 0 = 0
stirling n k
  | n < k     = 0
  | otherwise = stirling (n - 1) (k - 1) + k * stirling (n - 1) k
