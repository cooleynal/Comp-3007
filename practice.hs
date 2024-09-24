-- now can you think of how we could do sum columns from A2
-- using this
-- and map



-- > (!!) [1, 2, 3] 2
-- 3
-- ghci> mf = flip (!!) 1
-- map myFunc [[1, 2, 3], [4,5,6]]
-- ghci> mf [1, 2, 3, 4, 5]
-- 2


mf = flip (!!) 1
map mf [[1, 2, 3], [4,5,6]]
sum $ map mf
sum $ map mf [[1, 2, 3], [4,5,6]]
7



avgGrade = (flip div 2) . sum . map mf
avgGrade [[1, 2, 3], [5, 6, 7]]






myFunc :: (a -> Bool) -> [a] -> [a]
myFunc f [] = []
myFunc f (x: xs) = if f x
  then x : myFunc f xs
  else myFunc f xs



arguments a function that takes some type and returns a bool and it takes a list of that same type
myFunc (\x->x==2) [1, 2, 3, 4]

myFunc (\x -> x == 2) [1, 2, 3, 4]
[2]

-- function that takes type its and returns a bool
myFunc :: (Int -> Bool) -> [Int] -> [Int]