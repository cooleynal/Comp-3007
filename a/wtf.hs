import List
import Prelude hiding (all, concat, filter, foldr, length, map, replicate, tail, zipWith)




-- module List (List, cons, nil, zipWith, foldr) where

-- import Prelude hiding (foldr, null, zipWith)

-- data List a = Nil | Cons a (List a) deriving (Show)

-- nil :: List a
-- nil = Nil

-- cons :: a -> List a -> List a
-- cons = Cons

-- null :: List a -> Bool
-- null Nil = True
-- null (Cons _ _) = False

-- zipWith :: (a -> b -> c) -> List a -> List b -> List c
-- zipWith f Nil _ = Nil
-- zipWith f _ Nil = Nil
-- zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

-- foldr :: (a -> b -> b) -> b -> List a -> b
-- foldr _ z Nil = z
-- foldr f z (Cons x xs) = f x (foldr f z xs)






-- Define your custom list type
-- data List a = Nil | Cons a (List a) deriving (Show)

-- Example of a list of lists (your `eg3`)
eg3 :: List (List Int)
eg3 = Cons (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))          -- First row
      (Cons (Cons 5 (Cons 6 (Cons 7 (Cons 8 Nil))))       -- Second row
      (Cons (Cons 9 (Cons 10 (Cons 11 (Cons 12 Nil))))    -- Third row
      Nil))




-- Function to get a specific row
getRow :: Int -> List (List a) -> List a
getRow 0 (Cons r _) = r  -- If it's the first row, return it
getRow n (Cons _ rest) = getRow (n - 1) rest  -- Move to the next row
getRow _ Nil = Nil  -- If the list of lists is empty, return Nil
