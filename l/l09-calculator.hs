--- Calculator

data Op = Pop | Push Double | Mult | Add | Neg | Div | Clear | None
  deriving (Show)

data Stack a = Stack [a] deriving (Show)

emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack l) = Stack $ x : l

pop :: Stack a -> Stack a
pop (Stack []) = Stack []
pop (Stack (_ : l)) = Stack l

top :: a -> Stack a -> a
top x (Stack []) = x
top _ (Stack (x : _)) = x

parse :: String -> Op
parse "p" = Pop
parse "*" = Mult
parse "+" = Add
parse "-" = Neg
parse "/" = Div
parse "c" = Clear
parse s =
  if all (`elem` "0123456789") s
    then Push ((read s) :: Double)
    else None

applyOp :: Op -> Stack Double -> Stack Double
applyOp Pop s = pop s
applyOp (Push n) s = push n s
applyOp Mult s = applyOp2 (*) s
applyOp Add s = applyOp2 (+) s
applyOp Neg s = applyOp1 (0 -) s
applyOp Div s = applyOp2 (/) s
applyOp Clear s = emptyStack
applyOp None s = s

applyOp2 f s =
  let x1 = top 0 s
      x2 = top 0 (pop s)
   in push (f x1 x2) (pop $ pop s)

applyOp1 f s =
  push (f (top 0 s)) (pop s)

--- everything above is functional

loop :: Stack Double -> IO ()
loop s = do
  str <- getLine
  let Stack l = applyOp (parse str) s
  putStrLn $ "Stack: " ++ show l
  loop (Stack l)

main =
  do
    putStrLn "Stack is empty. Enter an operation."
    loop emptyStack
