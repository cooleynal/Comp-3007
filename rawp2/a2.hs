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

eg :: Dict
eg = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
eg1 :: Dict
eg1 = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt))


-- firstKey eg = "Bingo"
firstKey :: Dict -> String -- firstKey is a function with Dict input and String output
firstKey Mt = ""
firstKey (Entry k v p) = k

-- firstValue eg = "Bongo"
firstValue :: Dict -> String
firstValue Mt = ""
firstValue (Entry k v p) = v

-- removeFirst eg = Entry "Baz" "Ola" (Entry "Big" "Deal" Mt)
removeFirst :: Dict -> Dict
removeFirst Mt = Mt
removeFirst (Entry k v p) = p


stringify :: Dict -> String
stringify Mt = ""
stringify (Entry k v Mt)  = k ++ ":" ++ v
stringify (Entry k v p)   = k ++ ":" ++ v ++ "," ++ stringify p


-- rev eg = Entry "Bongo" "Bingo" (Entry "Ola" "Baz" (Entry "Deal" "Big" Mt))
rev :: Dict -> Dict
rev Mt = Mt
rev (Entry k v p) = Entry v k (rev p)

-- reverse list
-- rev2 eg
rev2 :: Dict -> Dict
rev2 dict = rev2h dict Mt

rev2h :: Dict -> Dict -> Dict
rev2h Mt acc = acc
rev2h (Entry k v p) acc = rev2h p (Entry k v acc)

rev3 :: Dict -> Dict
rev3 dict = rev3h dict Mt
  where
    rev3h Mt acc = acc
    rev3h (Entry k v p) acc = rev3h p (Entry k v acc)

rev4 :: Dict -> Dict
rev4 dict =
  let
    rev4h Mt acc = acc
    rev4h (Entry k v p) acc = rev4h p (Entry k v acc)
  in
    rev4h dict Mt


-- find "Baz" eg = "Ola"
-- find "Egaah" eg = ""
find :: String -> Dict -> String -- find is a function with two inputs and String output
find s Mt = ""
find s (Entry k v p)
  | s == k = v
  | s == v = k
  | otherwise = find s p

-- Replace all occurrences (as a key or a value) of badWord by "###"
-- censor "Ola" eg = Entry "Bingo" "Bongo" (Entry "Baz" "###"" (Entry "Big" "Deal" Mt))
-- censor "Baz" (censor "Ola" eg) = Entry "Bingo" "Bongo" (Entry "###" "###"" (Entry "Big" "Deal" Mt))
-- censor "Baz" eg

censor :: String -> Dict -> Dict
censor _ Mt = Mt
censor s (Entry k v p) = Entry (hider k) (hider v) (censor s p)
  where
    hider candidate =
      if candidate == s then "###"
      else candidate


censor1 :: String -> Dict -> Dict
censor1 s Mt = Mt
censor1 s (Entry k v p) = Entry (hider k) v (censor1 s p)
  where
    hider candidate
      | candidate == s = "###"
      | otherwise = candidate


-- remove "Bingo" eg
remove :: String -> Dict -> Dict
remove _ Mt = Mt
remove s (Entry k v p)
  | s == k || s == v  = remove s p
  | otherwise         = Entry k v (remove s p)


-- removeDoubles (Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Bingo" "Deal" Mt)))
-- = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" Mt)
removeDoubles :: Dict -> Dict
removeDoubles Mt = Mt
removeDoubles (Entry k v p) = Entry k v (remove k (removeDoubles p))

-- Specification:
-- 1) for every key k and dictionaries d1 and d2,
--    find k (combine d1 d2) = find k d1  -- if (find k d1) is not ""
--    find k (combine d1 d2) = find k d2  -- otherwise
-- 2) removeDoubles (combine d1 d2) = combine d1 d2

-- combine eg eg1
-- combine :: Dict -> Dict -> Dict
-- combine Mt Mt = Mt
-- combine Mt m2 = combine m2 Mt
-- combine (Entry k1 v1 p1) m2 = Entry k1 v1 (combine p1 m2)

combine :: Dict -> Dict -> Dict
combine Mt Mt = Mt
combine Mt m2 = m2
combine m1 Mt = m1
combine (Entry k1 v1 p1) m2 = Entry k1 v1 (combine p1 m2)


findr :: String -> Dict -> Bool
findr _ Mt = False
findr s (Entry k _ p)
    | s == k    = True
    | otherwise = findr s p

-- removeDoubles (combine eg eg1)



combiner :: Dict -> Dict -> Dict
combiner m1 m2 = removeDoubles ( combine m1 m2)



main :: IO ()
main = do
  putStrLn (firstKey eg)
  putStrLn (firstValue eg)
  putStrLn (stringify (removeFirst eg))
  putStrLn (stringify eg)
  putStrLn (stringify (rev eg))
  putStrLn (stringify (rev2 eg))
  putStrLn (stringify (rev3 eg))
  putStrLn (stringify (rev4 eg))
  putStrLn (find "Baz" eg)
  putStrLn (find "Ola" eg)
  putStrLn (stringify (censor1 "Baz" eg))
  putStrLn (stringify (remove "Baz" eg))