import Prelude
    ( String
    , Int, (+), (-), (*), (<)
    , Bool(..), (==), (&&), (||), not
    , div, rem, foldl1, fst, snd, undefined, IO, putStrLn, show, (++)
    , (^), ($)  -- Added ($) here
    )

-- DON'T TOUCH THE ABOVE!!!!!!!!!!!!!!!!!!!!!!!!!!

--------------------------------------------------

-- E.g. lastDigit 3007 = 7. n>=0.
lastDigit :: Int -> Int
lastDigit n =
    rem n 10

-- E.g. firstDigit 3007 = 3. n>=0.
-- Hint: div 3007 10 = 300.
firstDigit :: Int -> Int
firstDigit n =
    if n < 10 then n
    else firstDigit (div n 10)

-- fact n = n! = 1*2*...*n. n>=0.
fact :: Int -> Int
fact n = foldl1 (*) [1..n]

-- For n>0, sumFact n = 1! + 2! + ... + n!
sumFact :: Int -> Int
sumFact n =
    if n == 1 then 1
    else sumFact (n-1) + fact n

-- If a,b >= 1 then (log a b) is the largest k such that a^k <= b.
log :: Int -> Int -> Int
log a b =
    if b < a then 0
    else 1 + log a (div b a)

-- E.g. numDigits 3007 = 4. n>=0.
numDigits :: Int -> Int
numDigits n =
    if n < 10 then 1
    else 1 + numDigits (div n 10)

-- E.g. reverseDigits 3007 = 7003. n>=0.
reverseDigits :: Int -> Int
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
-- your function. Since you only have z, s and iterfun to work with in defining
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
    x == 0 || p x == x - 1

main :: IO ()
main = do

    putStrLn $ "lastDigit 3007: " ++ show (lastDigit 3007)


    putStrLn $ "firstDigit 3007: " ++ show (firstDigit 3007)

    putStrLn $ "fact 5: " ++ show (fact 5)

    putStrLn $ "sumFact 5: " ++ show (sumFact 5)

    putStrLn $ "log 3 11: " ++ show (log 3 11)

    putStrLn $ "numDigits 3007: " ++ show (numDigits 3007)

    putStrLn $ "reverseDigits 3007: " ++ show (reverseDigits 3007)

    putStrLn $ "p 5: " ++ show (p (5 :: Int))
    putStrLn $ "p 0: " ++ show (p (0 :: Int))

    putStrLn $ "pTest 5: " ++ show (pTest (5 :: Int))
    putStrLn $ "pTest 0: " ++ show (pTest (0 :: Int))
