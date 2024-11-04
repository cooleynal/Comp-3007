
a :: Int -> Int
a n
  | n == 0    = 1
  | n == 1    = 0
  | otherwise = 4 * a (n - 1) - 4 * a (n - 2) + (-1)^n

b :: Int -> Double
b n = ((-1) ^ n) * (1 / 9) + ((8/ 9) - (5 / 6) * fromIntegral n) * 2 ^ n



c :: Int -> Int
c 0 = 1
c 1 = 3
c n = 3 * c (n - 1) - 2 * c (n - 2)



d :: Int -> Double
d n = 2^(n + 1) - 1



e :: Int -> Double
e 0 = 0
e n = e (n - 1) + (fromIntegral n)^2


f :: Int -> Double
f n = (fromIntegral n + 1) * (fromIntegral n) * (2*(fromIntegral n) + 1)/ 6



main :: IO ()
main = do
    let numTerms = 10

    print $ map a [0..numTerms - 1]

    print $ map b [0..numTerms - 1]

    putStrLn ""

    print $ map c [0..numTerms - 1]

    print $ map d [0..numTerms - 1]


    putStrLn ""

    print $ map e [0..numTerms - 1]

    print $ map f [0..numTerms - 1]
