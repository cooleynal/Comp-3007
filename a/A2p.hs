
data Dict = Mt | Entry String String Dict
  deriving (Show)

eg = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
eg1 = Entry "Bingo1" "Bongo1" (Entry "Baz1" "Ola1" (Entry "Big1" "Deal1" Mt))


-- find "Baz" eg = "Ola"
-- -- find "Egaah" eg = ""
find :: String -> Dict -> String
find _ Mt = ""
find target_key (Entry key value point)
    | target_key == key = value
    | otherwise = (find target_key point)


d1 :: Dict
d1 = Entry "Bingo" "Bongo" (Entry "Baz" "Ola" (Entry "Big" "Deal" Mt))
d2 :: Dict
d2 = Entry "Bingo1" "Bongo1" (Entry "Baz" "Ola" (Entry "Big1" "Deal1" Mt))

combine :: Dict -> Dict -> Dict
combine Mt d2 = d2
combine (Entry key value point) d2 =
    let combinedRest = combine point d2
    in if find key d2 == "" then Entry key (if value == "" then find key d2 else value) combinedRest else combinedRest


-- ghci> example 2 "-"
example :: Int -> String -> String
example x sign
    | x > 0 && sign == "-"   = "Negative"
    | x == 0 && sign == ""   = "Zero"
    | x > 0  && sign == ""   = "Positive"