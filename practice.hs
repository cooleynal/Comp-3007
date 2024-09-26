-- module Main where
import Data.List (sortBy)
import Data.Ord (comparing)

-- now can you think of how we could do sum columns from A2
-- using this
-- and map



-- > (!!) [1, 2, 3] 2
-- 3
-- ghci> mf = flip (!!) 1
-- map mf [[1, 2, 3], [4,5,6]]

-- ghci> mf [1, 2, 3, 4, 5]
-- 2


-- mf = flip (!!) 1
-- mf1 = map mf [[1, 2, 3], [4,5,6]]

-- sum $ mf1
sumColumns :: [[Int]] -> [Int]
sumColumns rows = foldr (zipWith (+)) (replicate (length (head rows)) 0) rows





-- avgGrade = (flip div 2) . sum . map mf
-- avgGrade [[1, 2, 3], [5, 6, 7]]






myFunc :: (a -> Bool) -> [a] -> [a]
myFunc f [] = []
myFunc f (x: xs) = if f x
  then x : myFunc f xs
  else myFunc f xs



-- arguments a function that takes some type and returns a bool and it takes a list of that same type
-- myFunc (\x->x==2) [1, 2, 3, 4]

-- myFunc (\x -> x == 2) [1, 2, 3, 4]
-- [2]

-- function that takes type its and returns a bool
-- myFunc :: (Int -> Bool) -> [Int] -> [Int]


names :: [(String, String)]
names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Summer"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
  ]



-- lp :: [(String, String)] -> [(String, String)]
-- lp [] = []
-- lp (a:b) = (snd a) : lp b


-- lp :: [(String, String)] -> [(String, String)]
-- lp [] = []
-- lp (a:b) =  a : lp b


lp :: [(String, String)] -> [String]
lp [] = []
lp ((_, a2):b) = a2 : lp b



myTuple :: (String, String)
myTuple = ("Ian", "Curtis")

lastName :: String
(_, lastName) = myTuple


-- cln ("Ian", "Curtis") ("Bernard", "Summer")
cln :: (String, String) -> (String, String) -> Ordering
cln (a1, a2) (b1, b2)
  | a2 < b2   = LT
  | a2 > b2   = GT
  | otherwise = EQ



-- sbs names names
sbs :: [(String, String)] -> [(String, String)]
sbs = sortBy cln

sbs1 :: [(String, String)] -> [(String, String)]
sbs1 = sortBy (comparing snd)




-- mfz = zz 2
-- mfz 11
-- 13
zz :: Num a => a -> a -> a
zz x y = x + y