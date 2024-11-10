


-- type Name = String
-- type Age = Int

-- type People = (Name, Age)

-- p1 :: People
-- p1 = ("hi", 5)

-- p2 :: People
-- p2 = ("James", 5)

data Name = Name (String, String)
  -- deriving (Show)



p1 :: Name
p1 = Name ("bil", "billy1")

p2 :: Name
p2 = Name ("bil2", "billy2")

-- instance Show Name where
--   show "hello"


instance Show Name where
  show :: Name -> String
  show (Name (firstName, lastName)) = "firstname " ++ firstName ++ "\nlastname " ++ lastName

instance Eq Name where
  (==) :: Name -> Name -> Bool
  (Name (firstName1, lastName1)) == (Name (firstName2, lastName2)) = lastName1 == lastName2
  (/=) :: Name -> Name -> Bool
  (Name (firstName1, lastName1)) /= (Name (firstName2, lastName2)) = lastName1 /= lastName2


  -- equivDB db1 db2 = sortDB db1 == sortDB db2


instance Num Name where
  (+) :: Name -> Name -> Name
  (Name (firstName1, lastName1)) + (Name (firstName2, lastName2)) = Name (firstName1, lastName2 ++ lastName2)



type Re = Double
type Im = Double

data Complex = Complex (Re, Im)

instance Show Complex where
  show :: Complex -> String
  show (Complex (a, b)) = "Re: " ++ show a ++ "\nIm: " ++ show b


z1 :: Complex
z1 = Complex(1, 2.2)
z2 :: Complex
z2 = Complex(2, -2.1)

instance Num Complex where
  (+) :: Complex -> Complex -> Complex
  (Complex (a1, b1)) + (Complex (a2, b2)) = Complex (a1+a2, b1 +b2)
  -- (-) = _
  -- (*) = _
  -- negate = _
  -- abs = _
  -- signum = _
  -- fromInteger = _

-- ord requires eq
-- ghci> :i Ord
instance Ord Complex where
  compare :: Complex -> Complex -> Ordering
  compare (Complex (a1, b1)) (Complex (a2, b2)) = compare a1 a2

--   ghci> :i Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}

-- removes error above "INSTANCE EQ "
-- ghci> :i Eq
instance Eq Complex where
  (==) :: Complex -> Complex -> Bool
  -- (==) (Complex (a1, b1)) (Complex (a2, b2)) =
  --   a1 == a2 && b1 == b2
  (Complex (a1, b1)) == (Complex (a2, b2)) =
    a1 == a2 && b1 == b2

-- (length lastName1) + (length lastName2)

-- func (Eq => a) Int a -> Int b

-- Ord a => a -> a -> a
-- a must be a instance of the Order class


-- sortDB :: (Ord a) => [a] -> [a]
-- sortDB (rows) =  (simpleSort rows)
--   where
--     simpleSort [] = []
--     simpleSort (x : xs) = insert x (simpleSort xs)

--     insert y [] = [y]
--     insert y (z : zs)
--       | orderRows y z == LT = y : z : zs
--       | otherwise = z : insert y zs

-- holes
-- gf :: Int -> Int -> Int
-- gf a b = _c

data BloodType = A | B | O
  deriving (Eq, Show)

instance Ord BloodType where
   (<) :: BloodType -> BloodType -> Bool
   A < B = True
   min :: BloodType -> BloodType -> BloodType
   (min) A B = A


