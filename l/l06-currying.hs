


myMap :: (a -> b) -> [a] -> [b]
myMap f l =
  let op x z = f x : z
   in foldr op [] l


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p l =
  let op x z = if p x then x : z else z
   in foldr op [] l

rev :: [a] -> [a]
rev l = foldr snoc [] l
  where
    snoc x l = l ++ [x]


main :: IO ()
main = do
  -- Example usage of myMap
  let numbers = [1, 2, 3, 4, 5]
  let doubled = myMap (*2) numbers
  putStrLn "Original list:"
  print numbers
  putStrLn "Doubled list using myMap:"
  print doubled

  -- Example usage of myFilter
  let evens = myFilter even numbers
  putStrLn "Filtered even numbers using myFilter:"
  print evens

  -- Example usage of rev
  let reversed = rev numbers
  putStrLn "Reversed list using rev:"
  print reversed
