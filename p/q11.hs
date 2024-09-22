import Prelude
    (String
    ,Int, (+), (-), (*),  (<)
    ,(^)       -- integer exponentiation; 2^4 = 16
    ,div, rem  -- integer division and remainder; div 11 3 = 3, rem 11 3 = 2
    ,Bool(..), (==), (&&), (||), not
    ,foldl1
    ,fst, snd
    ,undefined
    )

{-^!#!^-}

-- DON'T TOUCH THE ABOVE!!!!!!!!!!!!!!!!!!!!!!!!!!

--------------------------------------------------


-- E.g. lastDigit 3007 = 7. n>=0.
lastDigit :: Int -> Int
lastDigit n = n - (n `div` 10 * 10)


-- E.g. firstDigit 3007 = 3. n>=0.
-- Hint: div 3007 10 = 300.
firstDigit :: Int -> Int
firstDigit n =
    if n < 10 then n
    else firstDigit (n `div` 10)

-- fact n = n! = 1*2*...*n. n>=0.
-- For use in sumFact.
fact n = foldl1 (*) [1..n]


sumFact :: Int -> Int
sumFact n
    -- | n < 0    = 1  -- Base case for 0!
    | n < 2    = 1  -- Base case for 1!
    | True      = fact n + sumFact (n - 1)

-- If a,b >= 1 then (log a b) is the largest k such that a^k <= b.
-- For example, (log 3 11) = 2 since 3^2 = 9 <= 11 < 27 = 3^3
log :: Int -> Int -> Int
log a b =
    if b < a then 0
    else 1 + log a (b `div`a )

-- E.g. numDigits 3007 = 4. n>=0.
numDigits n =
    if n<10 then 1
    else 1 + numDigits (n `div` 10)

-- E.g. reverseDigits 3007 = 7003. n>=0.
reverseDigits n =
    if n < 10 then n
    else reverseDigits (div n 10) + lastDigit n * 10 ^ (numDigits n - 1)



--
-- EXTRA

class S a where
    z :: a
    s :: a -> a
    iterfun :: (b -> b) -> a -> b -> b

-- This declaration says that Int is one of the types in the class S because we
-- define the three required functions as shown. Because of this declaration, if
-- the compiler sees (s x) where x is of type Int, it will use the s defined
-- here.
instance S Int where
    z = 0
    s = (+1)
    iterfun f n x =
        if n == 0 then x
        else iterfun f (n-1) (f x)

-- Define the "minus one" function for types in the class S. Use pTest to test
-- your function. Since you onl1y have z, s and iterfun to work with in defining
-- p, it's not straightforward. You might consider using pairing ( (,), fst,
-- snd).
p :: S a => a -> a
p x =
    snd (iterfun p' x (True, z))

p' :: S a => (Bool, a) -> (Bool, a)
p' x =
    if fst x then (False, snd x)
    else (False, s (snd x))

pTest :: Int -> Bool
pTest x =
    x==0 || p x == x - 1