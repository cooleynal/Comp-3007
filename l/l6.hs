-- Find op, z with [f x1, ..., f xn] = x1 `op` (x2 `op` ... `op` (xn `op` z))
myMap :: (a -> b) -> [a] -> [b]
myMap f l =
  let op x z = f x : z
   in foldr op [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p l =
  let op x z = if p x then x : z else z
   in foldr op [] l

-- rev [x1, ..., xn] = [xn, ..., x1]
-- Find f, z with [xn, ..., x1] = x1 `f` (x2 `f` ... `f` (xn `f` z))
rev l = foldr snoc [] l
  where
    snoc x l = l ++ [x]
