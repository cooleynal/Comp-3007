
import List
import Prelude hiding (all, concat, filter, foldr, length, map, replicate, tail, zipWith)


eg = cons 1 (cons 2 (cons 3 nil))
eg1 = cons 5 (cons 66 (cons 44 nil))

eg3 :: List (List Int)
eg3 = cons        (cons 1 (cons 2 (cons 3 (cons 4 nil))))          -- First row
           (cons  (cons 5 (cons 6 (cons 7 (cons 8 nil))))    -- Second row
           (cons  (cons 9 (cons 10 (cons 11 (cons 12 nil)))) -- Third row
            nil))

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
all f l = foldr fapply True l
  where fapply ele acc = f ele


append :: List a -> List a -> List a
append l1 l2 = foldr cons l2 l1

concat :: List (List a) -> List a
concat ls = foldr append nil ls

length :: List a -> Int
length l = foldr fapply 0 l
  where
    fapply ele acc = acc + 1

-- split p l: return a pair of lists lt and lf containing, respectively, the
-- elements of l where p is true/false.
-- split (== 2) eg = (Cons 2 Nil, Cons 1 (Cons 3 Nil))
split :: (a -> Bool) -> List a -> (List a, List a)
split p l = foldr splitter (nil, nil) l
  where
    splitter ele (left, right)
      | p ele = (cons ele left, right)
      | otherwise = (left, cons ele right)



-- replicate 4 17 = Cons 17 (Cons 17 (Cons 17 (Cons 17 Nil)))
replicate :: Int -> a -> List a
replicate n x | n <= 0 = nil
replicate n x = cons x $ replicate (n - 1) x


sumColumns :: Int -> List (List Int) -> List Int
sumColumns n l = foldr sumer (replicate n 0) l
  where sumer row acc = zipWith (+) row acc


tail :: List a -> List a
tail l = foldr addToTail nil l
  where
    size = length l
    addToTail x acc
      | length acc < (size - 1) = cons x acc
      | otherwise = acc



main :: IO ()
main = do

  print $ map f eg
  print ""

  print $ filter (fm 0) eg
  print ""

  print $ all(fm 1) eg
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
