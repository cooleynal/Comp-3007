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




-- List = 1:2:3:[]

data Dict = Mt | Entry String String Dict
  deriving (Show)

eg1 = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
eg2 = Entry "Bingo1" "Bongo1" (Entry "Baz" "Ola" (Entry "Big1" "Deal1" (Entry "rrrBig1" "rrDeal1" Mt)))


apr :: Dict -> Dict -> Dict
apr Mt m2 = m2
apr (Entry k1 v1 p1) m2 = Entry k1 v1 (apr p1 m2)


-- Entry "Bingo" "Bongo" rmd (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))

-- Entry "Bingo" "Bongo" (Entry "Baz" "Ola" rmd (Entry "Big" "Deal" Mt))

-- ghci> rmd (Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt)))


rmdHelper :: String -> Dict -> Dict
rmdHelper _ Mt = Mt
rmdHelper s (Entry k v p)
  | s == k = (rmdHelper s p)
  | otherwise = Entry k v (rmdHelper s p)



rmd :: Dict -> Dict
rmd Mt = Mt
rmd (Entry k v p) = Entry k v (rmd (rmdHelper k p))
-- rmd (Entry k v p) = Entry k v (rmdHelper k (rmd p))









-- removeKey "str" eg
-- removeKey :: String -> Dict -> Dict
-- removeKey s Mt = Mt
-- removeKey s (Entry k v p) = Entry k v (removeKey s p)


-- removeKey :: String -> Dict -> Dict
-- removeKey s Mt = Mt
-- removeKey s (Entry k v p)
--   |s == k = removeKey s p
--   |s /= k = Entry k v (removeKey s p)

-- removeKey :: String -> Dict -> Dict
-- removeKey s Mt = Mt
-- removeKey s (Entry k v p)
--   |s == k = removeKey s p
--   |otherwise = Entry k v (removeKey s p)



rever :: Dict -> Dict
rever Mt = Mt
rever (Entry k v p) = Entry v k (rever p)

-- reverseDict eg
-- reverseDict :: Dict -> Dict
-- reverseDict Mt = Mt
-- reverseDict (Entry k v p) = Entry k v (reverseDict p)



reverseDict :: Dict -> Dict
reverseDict Mt = Mt
reverseDict (Entry k v p) = append (Entry k v Mt) (reverseDict p)

append :: Dict -> Dict -> Dict
append Mt dict2 = dict2
append (Entry k v p) dict2 = Entry k v (append p dict2)






-- getsecond
getsecond :: Dict -> Dict
getsecond Mt = Mt
-- getsecond Entry s1 s2 p = "_" s2 : getsecond s1 s2 p
getsecond (Entry s1 s2 p) = Entry "s2" s1 (getsecond p)


-- apd eg eg1
apd :: Dict -> Dict -> Dict
apd Mt (Entry k2 v2 p2) = Entry k2 v2 (apd Mt p2)
apd Mt Mt = Mt
apd (Entry k1 v1 p1) x = Entry k1 v1 (apd p1 x)


data DB = DB [[Int]] deriving (Show)
db :: DB
db =
  DB
    [ [1, 2075, 6, 1271, 1930]
    , [2, 254, 239, 65, 571]
    , [3, 1510, 301, 1570, 703]
    , [4, 271, 2009, 2085, 1899]
    , [5, 1974, 449, 1975, 580]
    , [6, 1329, 1970, 1152, 608]
    , [7, 2019, 724, 468, 953]
    , [8, 1404, 2155, 1671, 324]
    , [9, 1714, 1135, 738, 959]
    , [10, 1223, 1398, 1466, 876]
    ]

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