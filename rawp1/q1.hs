-- Quiz 1
-- COMP 3007 Fall 2024

-- The quiz has five questions each worth 10 points. Gradescope will autograde
-- it. Sutmit as many times as you like before time is up. Unless a regrade
-- request changes your score, the autograder's grade is final.
--
-- At the bottom of the file is some data you can test your code on, and some
-- examples of running the required functions.

---------------------------------------------------------------------------------

-- From A1, except "Eq" is added so that dictionaries can be tested for
-- equality. You don't need that. It's just for the qutograder (but feel free to
-- use it).
data Dict = Mt | Entry String String Dict
  deriving (Show, Eq)

-- From A2 but simplified: no DB constructor. DB is just a name for [[Int]].
type DB = [[Int]]

-- Feel free to use this in your code.
member :: String -> Dict -> Bool
member x Mt = False
member x (Entry y _ d) | x == y = True
member x (Entry _ _ d) = member x d


-- dict1 = Entry "a" "ab" (Entry "k" "ba" (Entry "b" "ka" (Entry "z" "ba" Mt)))

-- pre v d: a list of all keys in d whose corresponding value is v
-- pre "ba" dict1

pre :: String -> Dict -> String
pre _ Mt = ""
pre target_value (Entry key value point) =
  if target_value == value
    then key ++ pre target_value point
  else pre target_value point


  -- pre2 "ba" dict1
pre2 :: String -> Dict -> String
pre2 _ Mt = ""
pre2 target_value (Entry key value point)
  | target_value == value = key ++ (pre2 target_value point)
  | otherwise             = (pre2 target_value point)


--   -- pr3 "ba" dict1
-- pre3 :: String -> Dict -> [String]
-- pre3 _ Mt = [""]
-- pre3 target_value (Entry key value point)
--   | target_value == value = [key]  (pre3 target_value point)
--   | otherwise             = (pre3 target_value point)

-- pre3 "ba" dict1
pre3 :: String -> Dict -> [String]
pre3 _ Mt = []
pre3 target_value (Entry key value point)
  | target_value == value = key : (pre3 target_value point)
  | otherwise             = (pre3 target_value point)


-- ghci> 1 : [2, 3, 4]
-- [1,2,3,4]
-- ghci> 'a' : "bc"
-- "abc"

-- ghci> [1, 2, 3] ++ [4,5,6]
-- [1,2,3,4,5,6]
-- ghci>

-- difference d1 d2: like d1, but remove any key (with its value) that is also in d2

-- difference dict1 dict2
difference :: Dict -> Dict -> Dict
difference Mt _ = Mt
difference _ Mt = Mt
difference (Entry key1 value1 point1) (Entry key2 value2 point2)
  | key1 == key2 = difference point1 point2 -- recursive call
  | otherwise = Entry key1 value1 (difference point1 (Entry key2 value2 point2)) -- create new key and value and point

-- d dict1 dict2
d :: Dict -> Dict -> Dict
d Mt _ = Mt
d _ Mt = Mt
d (Entry k1 v1 p1) (Entry k2 v2 p2) =
  if k1 == k2 then d p1 p2
  else Entry k1 v1 (d p1 (Entry k2 v2 p2))


-- loop dict1
loop :: Dict -> [String]
loop Mt = []
loop (Entry key value point) = (key ++ " " ++ value) : loop point



-- pre3 "ba" dict1
-- Like the GetRow query in A2
select :: [[Int]] -> Int -> [Int]
select (r : rows) n | head r == n = r
select (_ : rows) n = select rows n
select _ _ = []

-- Replace all negative integers by 0. Assume the DB is valid.
-- unNeg db1
unNeg :: DB -> DB
unNeg [] = []
unNeg (row : rows) = toZero row : unNeg rows
  -- | tf row      = row : unNeg rows
  -- | otherwise   = unNeg rows

-- toZero [1,2075,6,-1271,1930]
toZero :: [Int] -> [Int]
toZero [] = []
toZero (f : r)
  | f > 0 = f : toZero r
  | f < 1 = 0 : toZero r

-- replace negative by 0 in single list [ x ]

-- Remove any rows that contain at least one negative value.  Assume the DB is valid.
-- deNeg db1
deNeg :: DB -> DB
deNeg [] = []
deNeg (row: rows) =
  if tf row then row : deNeg rows
  else deNeg rows


unNeg1 :: DB -> DB
unNeg1 [] = []
unNeg1 (row : rows)
  | tf row      = row : unNeg1 rows
  | otherwise   = unNeg1 rows




tf :: [Int] -> Bool
tf [] = True
tf list = foldl (&&) True (map (>0) list)




-- foldl (+) 0 [1,2075,6,1271,1930]

-- foldl (&&) True (map (>3) [1,2075,6,1271,1930])

-- ghci> [1,2075,6,1271,1930] : [[2,254,-239,65,571]]
-- [[1,2075,6,1271,1930],[2,254,-239,65,571]]


-- ghci> [1,2075,6,1271,1930] ++ [2,254,-239,65,571]
-- [1,2075,6,1271,1930,2,254,-239,65,571]

-- intersect db1 db2: like db1, except discarding any rows whose key is not in
-- db2. I.e. discard (from db1) any row [x,...] where select x db2 = [].  Assume
-- the DBs are valid.
-- intersect db1 db2
intersect :: DB -> DB -> DB
intersect [] _ = []
intersect (f1 : l1) b =
  let keys = genKeys b
    in
      if head f1 `elem` keys
        then f1 : intersect l1 b
        else intersect l1 b



-- genKeys db1
genKeys :: [[Int]] -> [Int]
genKeys [] = []
genKeys (f: r) = head f : genKeys r

-- isIn (genKeys db1) 5
isIn :: [Int] -> Int -> Bool
isIn [] _ = False
isIn (f : r) a
  | f == a = True
  | otherwise = isIn r a


-- equivDB (DB rows1) (DB rows2) =
--     all (`elem` rows2) rows1 && all (`elem` rows1) rows2

-----------------------
-- Examples for testing
-----------------------

dict1 =
  Entry "a" "ab" (Entry "k" "ba" (Entry "b" "ka" (Entry "z" "ba" Mt)))

dict2 =
  Entry "a" "ab" (Entry "b" "ka" (Entry "u" "uu" Mt))

db1 :: DB
db1 =
  [ [1, 2075, 6, 1271, 1930],
    [2, 254, -239, 65, 571],
    [3, 1510, 301, 1570, 703],
    [4, 271, 2009, -2085, 1899],
    [5, 1974, 449, 1975, 580]
  ]

db2 :: DB
db2 =
  [ [6, 1329, 1970, 1152, 608],
    [7, 2019, 724, 468, 953],
    [8, 1404, 2155, 1671, 324],
    [3, 510, 01, 1570, 703],
    [4, 71, 009, 2085, 1899],
    [9, 1714, 1135, 738, 959],
    [10, 1223, 1398, 1466, 876]
  ]

-----------------------
-- Example input-output
-----------------------
--
-- ghci> pre "ba" dict1
-- ["k","z"]
-- ghci> difference dict1 dict2
-- Entry "k" "ba" (Entry "z" "ba" Mt)
-- ghci> select db2 4
-- [4,71,9,-2085,1899]
-- ghci> unNeg db1
-- [[1,2075,6,1271,1930],[2,254,0,65,571],[3,1510,301,1570,703],[4,271,2009,0,1899],[5,1974,449,1975,580]]
-- ghci> db1
-- [[1,2075,6,1271,1930],[2,254,-239,65,571],[3,1510,301,1570,703],[4,271,2009,-2085,1899],[5,1974,449,1975,580]]
-- ghci> deNeg db1
-- [[1,2075,6,1271,1930],[3,1510,301,1570,703],[5,1974,449,1975,580]]
-- ghci> intersect db1 db2
-- [[3,1510,301,1570,703],[4,271,2009,-2085,1899]]
