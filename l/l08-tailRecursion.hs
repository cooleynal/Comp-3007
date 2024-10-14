-- Filters out elements that satisfy the predicate p from a list
filterOut :: (a -> Bool) -> [a] -> [a]
filterOut p = filter (not . p)

-- Censors words by filtering out bad words
censor :: [String] -> [String] -> [String]
censor bads words =
  filter (not . (`elem` bads)) words  -- Using a point-free style

-- Data structure for a Tree
data Tree = Node Int [Tree]

-- Example tree
tree :: Tree
tree = Node 0 [Node 1 [], Node 2 [Node 3 [], Node 4 []], Node 5 []]

-- Converts a Tree to a list of strings for printing
treeLines :: Tree -> [String]
treeLines (Node n l) =
  (show n :) . map ("   " ++) . concat . map treeLines $ l

-- Prints the tree to the console
printTree :: Tree -> IO ()
printTree tree =
   let output = concat (map (++ "\n") (treeLines tree))
   in putStrLn output

-- Main function to demonstrate usage
main :: IO ()
main = do
  -- Print the example tree
  putStrLn "The tree structure is:"
  printTree tree

  -- Example of censoring words
  let badWords = ["bad", "ugly", "rude"]
  let wordsToCensor = ["good", "bad", "nice", "ugly", "pretty"]
  let censoredWords = censor badWords wordsToCensor
  putStrLn "\nCensored words:"
  print censoredWords

  -- Example of filterOut
  let numbers = [1, 2, 3, 4, 5, 6]
  let filteredNumbers = filterOut even numbers
  putStrLn "\nFiltered out even numbers:"
  print filteredNumbers
