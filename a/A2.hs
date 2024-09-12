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

-- Data type representing dictionaries. Ignore the "deriving" l1ine; it's added
-- here to enable ghci to print out objects in this type.
data Dict = Mt | Entry String String Dict
  deriving (Show)

eg = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
eg1 = Entry "Bingo1" "Bongo1" (Entry "Baz1" "Ola1" (Entry "Big1" "Deal1" Mt))

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
stringify :: Dict -> String
stringify Mt = ""
stringify (Entry key value point) = key ++ ":" ++ value ++ check point
    where
        check Mt = ""
        check r = "," ++ (stringify r)

-- rev eg = Entry "Bongo" "Bingo" (Entry "Ola" "Baz" (Entry "Deal" "Big" Mt))
rev :: Dict -> Dict
rev Mt = Mt
rev (Entry key value point) = Entry value key (rev point)


-- find "Baz" eg = "Ola"
-- -- find "Egaah" eg = ""
find :: String -> Dict -> String
find _ Mt = ""
find target_key (Entry key value point)
    | target_key == key = value
    | otherwise = (find target_key point)


-- Replace all occurrences (as a key or a value) of badWord by "###"
-- censor "Ola" eg = Entry "Bingo" "Bongo" (Entry "Baz" "###"" (Entry "Big" "Deal" Mt))
-- censor "Baz" (censor "Ola" eg) = Entry "Bingo" "Bongo" (Entry "###" "###"" (Entry "Big" "Deal" Mt))
censor :: String -> Dict -> Dict
censor _ Mt = Mt
censor badWord (Entry key value point) = Entry
    (if key == badWord then "###" else key)
    (if value == badWord then "###" else value)
    (censor badWord point)


-- remove "Baz" eg = Entry "Bingo" "Bongo" (Entry "Big" "Deal" Mt)
remove :: String -> Dict -> Dict
remove _ Mt = Mt
remove target_key (Entry key value point)
    | target_key == key = remove target_key point
    | otherwise = Entry key value
    (remove target_key point)


-- removeDoubles (Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt)))
-- = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" Mt)
removeDoubles :: Dict -> Dict
removeDoubles = collector [] where
    collector :: [String] -> Dict -> Dict
    collector _ Mt = Mt
    collector seenKeys (Entry key value point)
        | key `elem` seenKeys = (collector seenKeys point)
        | otherwise = Entry key value (collector (key : seenKeys) point)


d1 = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
d2 = Entry "Bingo1" "Bongo1" (Entry "Baz" "Ola1" (Entry "Big1" "Deal1" Mt))


-- Specification:
-- 1) for every key k and dictionaries d1 and d2,
--    find k (combine d1 d2) = find k d1  -- if (find k d1) is not ""
--    find k (combine d1 d2) = find k d2  -- otherwise
-- 2) removeDoubles (combine d1 d2) = combine d1 d2

-- find "Baz" (combine d1 d2)
-- find "Big1" (combine d1 d2)
-- removeDoubles (combine d1 d2)
combine :: Dict -> Dict -> Dict
combine Mt d2 = d2
combine (Entry key value rest) d2 = Entry key value (combine rest d2)
