-- Import the List module. The module header specifies what objects are
-- exported.
import List
-- Prelude is already imported, but importing it again with the "hiding" part
-- attached means the hidden objects are not imported. This lets us use the same
-- names for functions we define over our own List type.
import Prelude hiding (all, concat, filter, foldr, length, map, replicate, tail, zipWith)

-- DON'T TOUCH THE ABOVE! Make sure to delete any imports VSCode might add.
---------------------------------------------------------------------------

{-

Assignment 4
Due: Sunday Sept 29 23:59

The assignment is a bunch of exercises using `foldr`. To deter you from using recursion, we're using our own definition of the list type. The type is treated abstractly, in the sense that the type name and some operations are imported from the module List (in the file List.hs) and only the imported things can be used. The imported objects are:

- List       -- a data type of list with constructors Cons and Nil
- cons, nil  -- alternate names for the constructors
- zipWith    -- as in class, but redefined for our version of lists
- foldr      -- as with zipWith

These functions are all defined in List.hs. Note that the constructors `Cons` and `Nil` themselves are not in the list, so you can't do pattern-matching with them. However you can use `cons` and `nil` to build lists.

It is not impossible to, in effect, break this abstraction, but please don't do that. You won't get much (if any) value out of the exercises if you do.

All the functions needing implementation below are to be coded using foldr instead of recursion. Almost all the work is finding the right "op" and "z" to give as arguments to foldr.

-}


eg :: List Int
eg = cons 1 (cons 2 (cons 3 nil))

eg1 :: List Int
eg1 = cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 nil))))))

eg2 :: List Int
eg2 = cons 2 (cons 2 (cons 4 nil))

eg3 :: List (List Int)
eg3 = cons        (cons 1 (cons 2 (cons 3 (cons 4 nil))))          -- First row
           (cons  (cons 5 (cons 6 (cons 7 (cons 8 nil))))    -- Second row
           (cons  (cons 9 (cons 10 (cons 11 (cons 12 nil)))) -- Third row
            nil))





-- The List version of [1,2,3] is Cons 1 (Cons 2 (Cons 3 Nil)).
-- We need to use the functions cons and nil since the constructors Cons and Nil
-- are not exported from the List module.

map :: (a -> b) -> List a -> List b
map f l = foldr step nil l
    where
        step x acc = cons (f x) acc



filter :: (a -> Bool) -> List a -> List a
filter = undefined

isEven :: Integral a => a -> Bool
isEven x = x `mod` 2 == 0


-- all p xs: True iff p is true of every member of xs
-- all isEven eg2
all :: (a -> Bool) -> List a -> Bool
all f l = foldr step True l
  where
    step x acc = (f x) && acc


append :: List a -> List a -> List a
append l1 l2 = foldr cons l2 l1

-- concat eg3
concat :: List (List a) -> List a
concat ml = foldr append nil ml

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
