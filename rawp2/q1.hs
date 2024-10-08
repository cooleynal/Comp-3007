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

-- NEED TO CONCAT
-- pre :: String -> Dict -> String
-- pre _ Mt = ""
-- pre x (Entry k y d) = -- key value [string]
--   if x == y then k else (pre x d)

pre :: String -> Dict -> String
pre = undefined



-- difference d1 d2: like d1, but remove any key (with its value) that is also
-- in d2
-- difference dict1 dict2
difference :: Dict -> Dict -> Dict
difference = undefined



-- Like the GetRow query in A2
select :: [[Int]] -> Int -> [Int]
select (r : rows) n | head r == n = r
select (_ : rows) n = select rows n
select _ _ = []

-- Replace all negative integers by 0. Assume the DB is valid.
unNeg :: DB -> DB
unNeg = undefined
-- unNeg xs = filter (<0) xs
-- unNeg (x:xs) = unNeg xs

-- let rows = tail head rows
-- unNeg []
  -- DB (filter (\row -> head row y) rows)
-- unNeg = undefined

-- Remove any rows that contain at least one negative value.  Assume the DB is valid.
deNeg :: DB -> DB
deNeg = undefined



-- intersect db1 db2: like db1, except discarding any rows whose key is not in
-- db2. I.e. discard (from db1) any row [x,...] where select x db2 = [].  Assume
-- the DBs are valid.
intersect :: DB -> DB -> DB
intersect = undefined


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