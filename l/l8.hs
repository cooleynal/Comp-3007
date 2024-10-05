filterOut :: (a -> Bool) -> [a] -> [a]
filterOut p = filter (not . p)

censor :: [String] -> [String] -> [String]
censor bads words =
  filter (not . (flip elem bads)) words  -- "eta reduction" rewrite \x -> f x as f

  -- (flip elem bads) use (`elem` bads)

-- function "composition"
-- f . g = \x -> f (g x)

data Tree = Node Int [Tree]

tree :: Tree
tree = Node 0 [Node 1 [], Node 2 [Node 3 [], Node 4 []], Node 5 []]

treeLines :: Tree -> [String]

subTrees (Node n l) = l

treeLines (Node n l) = 
  ((:) (show n) . map ("   " ++) . concat . map treeLines ) l

printTree :: Tree -> IO()
printTree tree =
   let output = concat (map (++ "\n") (treeLines tree))
   in
   putStrLn output
