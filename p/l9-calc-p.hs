
data Op = Pop | Push Double | Multi | Add | Neg | Div | Clear | None
    deriving (Show)

data Stack a = Stack [a]
    deriving (Show)

emptyStack :: Stack a
emptyStack = Stack []


push :: a -> Stack a -> Stack a
push x (Stack l) = Stack  (x : l) -- cons x to l

pop :: Stack a -> Stack a
pop (Stack []) = Stack []
pop (Stack (_ : l)) = Stack l

applyOp :: Op -> Stack Double -> Stack Double
-- applyOp Pop s = pop s
applyOp (Push n) s = push n s

applyOp Add s = binOp (+) s
applyOp Neg s = binOp (-) s
applyOp Multi s = binOp (*) s
applyOp Div s = binOp (/) s

applyOp Clear s = emptyStack



top :: a -> Stack a -> a
top x (Stack []) = x
top _ (Stack (x : _)) = x



parse :: String -> Op
parse "*" = Multi
parse "/" = Div
parse "+" = Add
parse "-" = Neg

parse "p" = Pop
parse "" = Pop
parse "c" = Clear

parse s
    | all (`elem` "0123456789.") s = Push (read s :: Double)
    | otherwise = None




-- singOp :: Num a => (a -> a) -> Stack a -> Stack a
-- singOp f s =
--     push (f (top 0 s)) (pop s)


-- fetch the top 2 elements, replace top with f x1 x2
binOp :: Num a => (a -> a -> a) -> Stack a -> Stack a
binOp f s =
    let
        x1 = top 0 s
        x2 = top 0 (pop s)
        in
            push (f x1 x2) (pop (pop s))




loop :: Stack Double -> IO ()
loop s = do
    str <- getLine
    putStrLn $ "You entered: " ++ str
    let Stack l = applyOp (parse str) s
    putStrLn $ "Stack: " ++ show l
    loop (Stack l)


main :: IO ()
main =
    do
        putStrLn "Empty, Enter a value or operation:"
        loop emptyStack




rev :: String -> String
rev = foldl (flip (:)) ""