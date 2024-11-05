


data Map a b = Empty | Add a b (Map a b) deriving (Show,Eq)


type TotebagItems = String
data Totebag = Totebag (Map TotebagItems Int) deriving (Eq, Show)


data Ac = Ac
    { acId :: String
    , acSavings :: Int
    , acChequing :: Int
    , acVIP :: Bool
    }

-- Verbatim from Assignment 3.
apply :: Eq a => Map a b -> a -> Maybe b
apply Empty x =
    Nothing
apply (Add u v m) x =
    if x==u then Just v
    else apply m x

-- Verbatim from Assignment 3.
applyElse :: Eq a => Map a b -> a -> b -> b
applyElse m x d =
    case apply m x of
        Nothing -> d
        Just v  -> v

-- Verbatim from Assignment 3.
update :: Eq a => a -> b -> Map a b -> Map a b
update x y Empty =
    Add x y Empty
update x y (Add u v m) =
    if x==u then Add x y m
    else Add u v (update x y m)

-- EXAMPLES FOR TESTING

sampleMap1 :: Map String Int
sampleMap1 = Add "headphones" 1 (Add "shawarma" 17 (Add "phaser" 1 Empty))

sampleMap2 :: Map String Int
sampleMap2 = Add "earbuds" 1 (Add "shawarma" 4 (Add "BFG9000" 2 Empty))

sampleTotebag1 :: Totebag
sampleTotebag1 = Totebag sampleMap1

sampleTotebag2 :: Totebag
sampleTotebag2 = Totebag sampleMap2

-- Sample list of accounts for the last two questions.
acs :: [Ac]
acs =
    [ Ac "Archibald" 1700 0 False
    , Ac "Barnaby" 0 1700 False
    , Ac "Beatrix" 1700 1700 False
    , Ac "Bertram" 1800 1900 False
    , Ac "Genevieve" 0 0 True
    , Ac "Ludo" 0 0 False
    , Ac "Octavia" 323 444 False
    , Ac "Ottilie" 2 34 False
    , Ac "Sebastian" 0 13 False
    , Ac "Theodore" 0 17000000 False
    , Ac "Violet" 0 0 False
    , Ac "Xanthe" 555 777 False
    ]


member :: Eq a => a -> Map a b -> Bool
member a Empty          = False
member a (Add a' v r)   = a == a' || member a r

modify :: Eq a => (b -> b) -> a -> Map a b -> Map a b
modify f a Empty = Empty
modify f k (Add k' v r)
    | k == k'       = Add k (f v) r
    | otherwise     = Add k' v (modify f k r)



-- sums bag
-- combineTotebags sampleTotebag1 sampleTotebag2
combineTotebags :: Totebag -> Totebag -> Totebag
combineTotebags (Totebag tb1) (Totebag tb2) = Totebag $ combineMaps tb1 tb2
    where
        combineMaps :: Eq a => Map a Int -> Map a Int -> Map a Int
        combineMaps Empty m2 = m2
        combineMaps (Add k v p) m2
            | member k m2   = combineMaps p $ modify (+ v) k m2
            | otherwise = Add k v $ combineMaps p m2




totalBalances :: [Ac] -> [(String, Int)]
totalBalances acs = map totalBalance acs
    where
        totalBalance (Ac name balance1 balance2 _) = (name, balance1 + balance2)

-- a list of the deadbeats in an account list. A deadbeat is

-- someone who has no money in either savings or chequing *and* is *not* a VIP.

-- Example: deadbeats acs ≡ ["Ludo","Violet"].

-- b1 && b2 && False = deadbeat
deadbeats :: [Ac] -> [String]
deadbeats = map acId . filter ele
    where ele ac = not (acVIP ac) && acSavings ac == 0 && acChequing ac == 0


deadbeats' :: [Ac] -> [String]
deadbeats' = foldr ele []
    where
    ele ac acc -- account, accumulator
        | not (acVIP ac) && acSavings ac == 0 && acChequing ac == 0 = acId ac : acc
        | otherwise = acc
