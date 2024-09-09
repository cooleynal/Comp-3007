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
firstKey Mt = ""
firstKey (Entry key _ _) = key

-- firstValue eg = "Bongo"
firstValue :: Dict -> String
firstValue Mt = ""
firstValue (Entry _ value _) = value

-- removeFirst eg = Entry "Baz" "Ola" (Entry "Big" "Deal" Mt)
removeFirst :: Dict -> Dict
removeFirst Mt = Mt
removeFirst (Entry _ _ point) = point


-- stringify eg = "Bingo:Bongo,Baz:Ola,Big:Deal"
isEmpty :: Dict -> Bool
isEmpty Mt = True
isEmpty _ = False

stringify :: Dict -> String
stringify Mt = ""
stringify (Entry key value point) = key ++ ":" ++ value ++ if isEmpty point then "" else "," ++ stringify point

-- rev eg = Entry "Bongo" "Bingo" (Entry "Ola" "Baz" (Entry "Deal" "Big" Mt))
rev :: Dict -> Dict
rev Mt = Mt
rev (Entry value key point) = Entry key value point

-- find "Baz" eg = "Ola"
-- find "Egaah" eg = ""
find :: String -> Dict -> String -- find is a function with two inputs and String outpu
find _ Mt = ""
find key (Entry k v rest) | key == k  = v | otherwise = find key rest

-- Replace all occurrences (as a key or a value) of badWord by "###"
-- censor "Ola" eg = Entry "Bingo" "Bongo" (Entry "Baz" "###"" (Entry "Big" "Deal" Mt))
-- censor "Baz" (censor "Ola" eg) = Entry "Bingo" "Bongo" (Entry "###" "###"" (Entry "Big" "Deal" Mt))
censor :: String -> Dict -> Dict
censor _ Mt = Mt
censor badWord (Entry key value rest) =
  Entry (if key == badWord then "###" else key) (if value == badWord then "###" else value) (censor badWord rest)

-- remove "Baz" eg = Entry "Bingo" "Bongo" (Entry "Big" "Deal" Mt)
remove :: String -> Dict -> Dict
remove _ Mt = Mt
remove key (Entry k v rest) | key == k = remove key rest | otherwise = Entry k v (remove key rest)

-- removeDoubles (Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt)))
-- = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" Mt)
removeDoubles :: Dict -> Dict
removeDoubles = rd []
  where
    rd :: [String] -> Dict -> Dict
    rd _ Mt = Mt
    rd seen (Entry key value rest)
      | key `elem` seen = rd seen rest
      | otherwise = Entry key value (rd (key : seen) rest)

-- Specification:
-- 1) for every key k and dictionaries d1 and d2,
--    find k (combine d1 d2) = find k d1  -- if (find k d1) is not ""
--    find k (combine d1 d2) = find k d2  -- otherwise
-- 2) removeDoubles (combine d1 d2) = combine d1 d2
combine :: Dict -> Dict -> Dict
combine d1 d2 = combine' d1 d2
  where
    find :: String -> Dict -> String
    find _ Mt = ""
    find key (Entry k v rest)
      | key == k  = v
      | otherwise = find key rest
    combine' :: Dict -> Dict -> Dict
    combine' Mt d2 = d2
    combine' (Entry k v rest1) d2 =
      let inD2 = find k d2
      in if v /= ""
         then Entry k v (combine' rest1 d2)
         else Entry k inD2 (combine' rest1 d2)
