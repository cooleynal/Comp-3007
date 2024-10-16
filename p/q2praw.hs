

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


sampleMap1 :: Map String Int
sampleMap1 = Add "headphones" 1 (Add "shawarma" 17 (Add "phaser" 1 Empty))

sampleMap2 :: Map String Int
sampleMap2 = Add "earbuds" 1 (Add "shawarma" 4 (Add "BFG9000" 2 Empty))

sampleTotebag1 :: Totebag
sampleTotebag1 = Totebag sampleMap1

sampleTotebag2 :: Totebag
sampleTotebag2 = Totebag sampleMap2


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


--   member "shawarma" sampleMap1 ≡ True
--   member "car keys" sampleMap1 ≡ False
member :: Eq a => a -> Map a b -> Bool
member k Empty =
    False
member k (Add k' v m) =
    k == k' || member k m

--   modify (+1) "shawarma" sampleMap1
--   modify (+1) "car keys" sampleMap1
modify :: Eq a => (b -> b) -> a -> Map a b -> Map a b
modify f k Empty =
    Empty
modify f k (Add k' v m) | k == k' =
    Add k (f v) m
modify f k (Add k' v' m) =
    Add k' v' $ modify f k m


-- Example:
--   combineTotebags sampleTotebag1 sampleTotebag2
--   ≡ Totebag
--       (Add "headphones"
--             1
--             (Add "phaser"
--                  1
--                  (Add "earbuds"
--                       1
--                       (Add "shawarma"
--                            21
--                            (Add "BFG9000"
--                                  2
--                                  Empty)))))
--
combineTotebags :: Totebag -> Totebag -> Totebag
combineTotebags (Totebag m1) (Totebag m2) =
    Totebag $ combineMaps m1 m2

combineMaps :: Eq a => Map a Int -> Map a Int -> Map a Int
combineMaps Empty m = m
combineMaps (Add k n m1) m2 =
    if member k m2 then combineMaps m1 $ modify (+ n) k m2
    else Add k n $ combineMaps m1 m2


-- balance :: Ac -> Int
-- balance ac = acChequing ac + acSavings ac


totalBalances :: [Ac] -> [(String, Int)]
totalBalances = map toBalance
    where
        toBalance ac = (acId ac, balance ac)
        balance ac = acChequing ac + acSavings ac


deadbeats :: [Ac] -> [String]
deadbeats =
    map acId . filter isDeadbeat
    where isDeadbeat ac = not (acVIP ac) && acSavings ac == 0 && acChequing ac == 0
