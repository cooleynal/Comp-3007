
import List
import Prelude hiding (all, concat, filter, foldr, length, map, replicate, tail, zipWith)


-- The List version of [1,2,3] is Cons 1 (Cons 2 (Cons 3 Nil)).
-- We need to use the functions cons and nil since the constructors Cons and Nil
-- are not exported from the List module.
eg = cons 1 (cons 2 (cons 3 nil))


f :: Num a => a -> a
f a = a + 1



map :: (a -> b) -> List a -> List b
-- function ele, initial value nil, a list l
map f l = foldr fapply nil l
-- x is current element in list, z is the accumulated result
    where fapply ele acc = cons (f ele) acc




fm :: (Ord a, Num a) => a -> a -> Bool
fm c a
    | a > c     = True
    | otherwise = False


filter :: (a -> Bool) -> List a -> List a
filter f l = foldr fapply nil l
  where
    fapply ele acc
      | f ele  = cons ele acc
      | otherwise = acc





-- all p xs: True iff p is true of every member of xs
all :: (a -> Bool) -> List a -> Bool
all = undefined

append :: List a -> List a -> List a
append = undefined

concat :: List (List a) -> List a
concat = undefined

length :: List a -> Int
length = undefined

-- split p l: return a pair of lists lt and lf containing, respectively, the
-- elements of l where p is true/false.
-- split (== 2) eg = (Cons 2 Nil, Cons 1 (Cons 3 Nil))
split :: (a -> Bool) -> List a -> (List a, List a)
split = undefined

-- replicate 4 17 = Cons 17 (Cons 17 (Cons 17 (Cons 17 Nil)))
replicate :: Int -> a -> List a
replicate n x | n <= 0 = nil
replicate n x = cons x $ replicate (n - 1) x

-- A `row` is a List of integers. Given a non-empty List `rows` where each row
-- has length n, (sumColumns n rows) adds the rows together. Example using
-- ordinary list notation:
-- sumColumns [ [1,2,3], [1,1,1], [1,1,3] ] = [3, 4, 7]
sumColumns :: Int -> List (List Int) -> List Int
sumColumns = undefined

-- tail eg = Cons 2 (Cons 3 Nil)
-- This looks like it should be trivial, and it is with pattern matchine, but
-- it's suprisingly challenging to do with foldr.
tail :: List a -> List a
tail = undefined





main :: IO ()
main = do

  let q1 = map f eg
  print q1

  print ""

  let q2 = filter (fm 0) eg
  print q2
