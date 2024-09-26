module List (List, cons, nil, zipWith, foldr) where

import Prelude hiding (foldr, null, zipWith)

data List a = Nil | Cons a (List a) deriving (Show)

nil :: List a
nil = Nil

cons :: a -> List a -> List a
cons = Cons

null :: List a -> Bool
null Nil = True
null (Cons _ _) = False

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)

foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil = z
foldr f z (Cons x xs) = f x (foldr f z xs)
