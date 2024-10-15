



append :: [a] -> [a] -> [a]
append x y = x ++ y


eg :: String
eg = append ("a" ++ "b") "c"


addThree :: Int -> Int
addThree a = a+ 3


result :: [Int]
result = map addThree [1, 2, 3]


-- filter` isEven [1, 2, 3]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

isEven :: (Integral a) => a -> Bool
isEven num
    | num `mod` 2 == 0      = True
    | otherwise             = False

    -- safeDiv 3 0
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
