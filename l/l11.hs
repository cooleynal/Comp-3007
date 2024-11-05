import Data.Convertible

class Zero a where
  zero :: a

instance Zero Int where
  zero = 0

instance Zero [a] where
  zero = []

instance (Zero a, Zero b) => Zero (a, b) where
  zero = (zero, zero)

instance Zero Bool where
  zero = False

instance Zero (Maybe a) where
  zero = Nothing

myLookup :: (Eq a, Zero b) => a -> [(a, b)] -> b
myLookup x l = case lookup x l of
  Just x -> x
  Nothing -> zero

foo :: Int -> Bool
foo n =
  if n == zero
    then zero
    else null (n : zero)
