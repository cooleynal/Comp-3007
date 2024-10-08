-- Assignment 2
-- Due 23:59 Sunday Sept 15
--
-- The assignment is to write a bunch of small functions using the Dict (for
-- "Dictionary") data type defined below.
--
-- The only parts of the Haskell language you need are the following:
--   - pattern-matching equations with recursion, as in the lectures
--   - if-then-else expressions
--   - strings e.g. "I am a string because I'm surrounded by quotes."
--   - the operation == to test if two string are equal
--   - the operation ++ to concatenate strings, e.g. "foo" ++ "baz" has value
--   "foobaz"
--
--  For each function f you are to write, there will be line
--     f = undefined
--  Replace this line by your own code. This line is included so that every
--  function has a definition, otherwise the autograder will barf. Evaluating
--  "undefined" produces an error.
--
--  We also include types for all the functions below. This is not necessary for
--  the code to work, but they're good  documentation.
--
--  Submit by uploading to Gradescope. The file will be instantly autograded. You can
--  resubmit as many times as you like. The autograder will usually *not* give
--  diagnostics, i.e. you won't be able to use it as a debugging tool.

-- Data type representing dictionaries. Ignore the "deriving" line; it's added
-- here to enable ghci to print out objects in this type.
data Dict = Mt | Entry String String Dict
  deriving (Show)

eg = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))

-- firstKey eg = "Bingo"
firstKey :: Dict -> String -- firstKey is a function with Dict input and String output
firstKey Mt = []
firstKey (Entry f v p) = f

-- firstValue eg = "Bongo"
firstValue :: Dict -> String
firstValue Mt = []
firstValue (Entry f v p) = v

-- removeFirst eg = Entry "Baz" "Ola" (Entry "Big" "Deal" Mt)
removeFirst :: Dict -> Dict
removeFirst Mt = Mt
removeFirst (Entry f v p) = p
-- removeFirst = undefined

-- stringify eg = "Bingo:Bongo,Baz:Ola,Big:Deal"
stringify :: Dict -> String
stringify Mt = ""
stringify (Entry f v Mt) = f ++ ":" ++ v
stringify (Entry f v p)  = f ++ ":" ++ v ++ "," ++ stringify p
  -- | p == Mt     = f ++ ":" ++ v
  -- | otherwise   = f ++ ":" ++ v ++ "," ++ stringify p

-- rev eg = Entry "Bongo" "Bingo" (Entry "Ola" "Baz" (Entry "Deal" "Big" Mt))
rev :: Dict -> Dict
rev Mt = Mt
rev (Entry x y d) = Entry y x (rev d)

-- find "Baz" eg = "Ola"
-- find "Egaah" eg = ""
find :: String -> Dict -> String -- find is a function with two inputs and String output
find _ Mt = ""
find s (Entry k v p)
  | s == k = v
  | otherwise = find s p



-- Replace all occurrences (as a key or a value) of badWord by "###"
-- censor "Ola" eg = Entry "Bingo" "Bongo" (Entry "Baz" "###"" (Entry "Big" "Deal" Mt))
-- censor "Baz" (censor "Ola" eg) = Entry "Bingo" "Bongo" (Entry "###" "###"" (Entry "Big" "Deal" Mt))
censor :: String -> Dict -> Dict
censor s Mt = Mt
censor s (Entry k v p) =
  Entry (if k == s then "###" else k) (if v == s then "###" else v) (censor s p)

-- remove "Baz" eg = Entry "Bingo" "Bongo" (Entry "Big" "Deal" Mt)
-- remove :: String -> Dict -> Dict
-- remove x Mt = Mt
-- remove s (Entry k v p) =
--   if s == k
--     then remove s p
--   else
--     Entry k v (remove s p)


remove1 :: String -> Dict -> Dict
remove1 x Mt = Mt
remove1 s (Entry k v p)
  | s == k = remove1 s p
  | otherwise = Entry k v (remove1 s p)



myFunc :: (a -> Bool) -> [a] -> [a]
myFunc f [] = []
myFunc f (x: xs) = if f x
  then x : myFunc f xs
  else myFunc f xs




-- mpr :: (a -> b) -> [a] -> [b]
-- mpr _ [] = []
-- mpr ab (af : al)
--   | af == 5 = "55"

mpr :: (Eq a, Num a, Show b) => (a -> b) -> [a] -> [b]
mpr _ [] = []
mpr ab (af : al)
  | af == 5 = ab 55 : mpr ab al  -- Use ab to convert 55 to type b
  | otherwise = ab af : mpr ab al

  -- mpr :: (Eq a, Num a) => (a -> String) -> [a] -> [String]
  -- mpr _ [] = []
  -- mpr ab (af : al)
  --   | af == 5 = "55" : mpr ab al
  --   | otherwise = ab af : mpr ab al

-- mpr :: (Eq a, Num a) => (a -> b) -> [a] -> [b]
-- mpr _ [] = []
-- mpr ab (af : al)
--   | af == 5 = "55" : mpr ab al
  -- | otherwise = ab af : mpr ab al

-- myFunc2 :: [a] -> [a]
-- myFunc2 [] = []
-- myFunc2 (x: xs) = if (\x -> x == 2) x
--   then x : myFunc2 (\x -> x == 2) xs
--   else myFunc2 (\x -> x == 2) xs


remove :: String -> Dict -> Dict
remove _ Mt = Mt
remove s (Entry k v p)
  | s == k    = p
  | otherwise = Entry k v (remove s p)


-- removeDoubles (Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt)))
-- = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" Mt)
removeDoubles :: Dict -> Dict
removeDoubles Mt = Mt
removeDoubles (Entry k v p) =
  Entry k v (remove k ( removeDoubles p))



-- Specification:
-- 1) for every key k and dictionaries d1 and d2,
--    find k (combine d1 d2) = find k d1  -- if (find k d1) is not ""
--    find k (combine d1 d2) = find k d2  -- otherwise
-- 2) removeDoubles (combine d1 d2) = combine d1 d2
combine :: Dict -> Dict -> Dict
combine (Entry k1 v1 p1) p2 =
  Entry k1 v1 (remove k1 ( combine p1 p2))
