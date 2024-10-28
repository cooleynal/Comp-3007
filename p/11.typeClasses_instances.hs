

class Zero a where
  zero :: a

instance Zero Int where
  zero = 0

instance Zero [a] where
  zero = []

instance Zero Bool where
  zero = False

instance (Zero a, Zero b) => Zero (a, b) where
  zero = (zero, zero)

instance Zero (Maybe a) where
  zero = Nothing

myLookup :: (Eq a, Zero b) => a -> [(a, b)] -> b
myLookup x l = case lookup x l of
  Just x -> x
  Nothing -> zero



foo :: Int -> Bool
foo n =
  if n == zero then zero :: Bool
  else null (n : zero)


main :: IO ()
main = do
  putStrLn ""

  print (myLookup 1 [(1, 10), (2, 20), (3, 30)] :: Int)

  print (zero :: Int)

  print (zero :: [Int])

  print (zero :: (Int, Bool))
  print (zero :: (Bool, Int))

  print (zero :: (Maybe Int))

  print(foo (zero :: Int))
