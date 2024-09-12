data MyList = Empty | Cons String MyList
  deriving (Show)

firstElement :: MyList -> String
firstElement Empty = ""  -- Return an empty string if the list is empty
firstElement (Cons x _) = x  -- Return the first element if the list is non-empty


myEmptyList :: MyList
myEmptyList = Empty


myList = Cons "Hello" (Cons "World" Empty)
