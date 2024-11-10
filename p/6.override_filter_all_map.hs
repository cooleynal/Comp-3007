-- Hiding standard Prelude functions to avoid clashes
import Prelude hiding (all, concat, filter, foldr, length, map, replicate, tail, zipWith)

-- Define the custom List data type and constructors
data List a = Nil | Cons a (List a) deriving (Show, Eq)

-- Define nil and cons for convenience
nil :: List a
nil = Nil

cons :: a -> List a -> List a
cons x xs = Cons x xs

-- Define a custom foldr function for the List type
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ acc Nil = acc
foldr f acc (Cons x xs) = f x (foldr f acc xs)

-- Define zipWith for the custom List type
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- Example lists
eg = cons 1 (cons 2 (cons 3 nil))
eg1 = cons 5 (cons 66 (cons 44 nil))

eg3 :: List (List Int)
eg3 = cons (cons 1 (cons 2 (cons 3 (cons 4 nil))))           -- First row
           (cons (cons 5 (cons 6 (cons 7 (cons 8 nil))))     -- Second row
           (cons (cons 9 (cons 10 (cons 11 (cons 12 nil))))  -- Third row
            nil))

-- Function to add 1 to each element (example function for map)
f :: Num a => a -> a
f a = a + 1

-- Map function using foldr
map :: (a -> b) -> List a -> List b
map f l = foldr fapply nil l
  where fapply ele acc = cons (f ele) acc

-- Example filter predicate function
fm :: (Ord a, Num a) => a -> a -> Bool
fm c a
    | a > c     = True
    | otherwise = False

-- Filter function using foldr
filter :: (a -> Bool) -> List a -> List a
filter f l = foldr fapply nil l
  where
    fapply ele acc
      | f ele     = cons ele acc
      | otherwise = acc

-- All function to check if all elements satisfy a predicate
all :: (a -> Bool) -> List a -> Bool
all f l = foldr fapply True l
  where fapply ele acc = f ele && acc

-- Append two lists
append :: List a -> List a -> List a
append l1 l2 = foldr cons l2 l1

-- Concatenate a list of lists
concat :: List (List a) -> List a
concat ls = foldr append nil ls

-- Length of the list
length :: List a -> Int
length l = foldr fapply 0 l
  where fapply _ acc = acc + 1

-- Split list based on a predicate
split :: (a -> Bool) -> List a -> (List a, List a)
split p l = foldr splitter (nil, nil) l
  where
    splitter ele (left, right)
      | p ele     = (cons ele left, right)
      | otherwise = (left, cons ele right)

-- Replicate function
replicate :: Int -> a -> List a
replicate n x
  | n <= 0    = nil
  | otherwise = cons x (replicate (n - 1) x)

-- Sum columns of a matrix (List of Lists) element-wise
sumColumns :: Int -> List (List Int) -> List Int
sumColumns n l = foldr sumer (replicate n 0) l
  where sumer row acc = zipWith (+) row acc

-- Tail of the list (all elements except the first)
tail :: List a -> List a
tail Nil         = Nil
tail (Cons _ xs) = xs

-- Main function to test the code
main :: IO ()
main = do
  print $ map f eg
  print ""
  print $ filter (fm 0) eg
  print ""
  print $ all (fm 1) eg
  print ""
  print $ append eg eg1
  print ""
  print $ concat eg3
  print ""
  print $ length eg1
  print ""
  print $ sumColumns (length eg3) eg3
  print ""
  print $ tail eg3
  print ""
