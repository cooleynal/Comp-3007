


-- data List a = Nil | Cons a (List a)


-- return :: a -> M a
-- (>>=) :: m a -> (a -> m b) -> m b

-- return x = [x]
-- xs >>= f = concat (map f xs)

data Expr = Val Int | Div Expr Expr

-- datatype Expr
-- 2 constructors, Val and Div
-- expressions built up from integer values using division operator
--

-- 1            ->  val 1
-- 6 / 2        ->  Div (Val 6) (Val 2)
-- 6 / (3 / 1)  ->  Div (Val 6) ( Div (Val 3) (Val 1) )

-- diver (Div (Val 6) (Val 2))
diver :: Expr -> Int
diver (Val n) = n
diver (Div x y) = (diver x) `div` (diver y)


-- diver2 (Div (Val 6) (Val 2))
diver2 :: Expr -> Maybe Int
diver2 (Val n) = Just n
diver2 (Div x y) =
    case diver2 x of
        Nothing -> Nothing
        Just n -> case diver2 y of
            Nothing -> Nothing
            Just m -> safediv n m



  -- eval1 :: Expr -> Maybe Int
  -- eval1 (Val n) = return n
  -- eval1 (Div x y) = eval1 x >>= n ->
  --                   eval1 y >>= n ->
  --                   safediv n m))


-- THIS IS THE MONAD

-- eval (Div (Val 6) (Val 2))
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do
      n <- eval x
      m <- eval y
      safediv n m

-- Nothing is one of the constructors in the Maybe type
-- Just is one of the constructors in the Maybe type
safediv :: Int -> Int -> Maybe Int
safediv n m =
  if m == 0 then Nothing
else
  Just (n `div` m)

-- THIS IS ALSO THE MONAD
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m >>= f = case m of
  Nothing -> Nothing
  Just x  -> f x

-- a monad is some kind of type constructor like maybe or list, anything
-- together with 2 functions that has types
-- return and
-- >>=

-- return (or pure) — to inject a value into a monad.
-- >>= (bind) — to chain monadic operations together.

-- bridge between pure world of values a and impure world Maybe a things that go wrong
-- return :: a -> Maybe a
-- sequences gives us: something that can fail like Maybe, give it a function what to do with that a if a fails, to get back a Maybe b
-- >>= :: Maybe a -> (a -> Maybe b) -> Maybe b


-- return x = [x]
-- xs >>= f = concat (map f xs)

-- return :: a -> m a
-- (>>=) :: m a -> (a -> m b) -> m b


