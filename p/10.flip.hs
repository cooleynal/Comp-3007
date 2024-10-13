

import List
import Prelude hiding (all, concat, filter, foldr, length, map, replicate, tail, zipWith)


eg = cons 1 (cons 2 (cons 3 nil))


fm :: (Ord a, Num a) => a -> a -> Bool
fm a c
    | a > c     = True
    | otherwise = False


filter :: (a -> Bool) -> List a -> List a
filter f l = foldr fapply nil l
  where
    fapply ele acc
      | f ele  = cons ele acc
      | otherwise = acc









main :: IO ()
main = do


  print ""

  let q2 = filter (flip fm 1) eg
  print q2
