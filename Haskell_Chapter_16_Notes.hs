-- Haskell
--Chapter 16
-- Lazy Evaluation


{-

- Equational Reasoning
- Induction
- Proving Correctness of Simple Compiler


-}

------------------ Equational Reasoning ------------------

{-
(x + a) (x + b) 
{ left distributivity } 
(x + a) x +(x + a) b
{ right distributivity } 
xx + ax + xb + ab
{ squaring } 
x^2 + ax + xb + ab
{ commutativity }
x^2 + ax + b x + ab
{ right distributivity }
x^2 +(a + b) x + ab


Using algebra can increase efficiency

x(y + z) = xy + xz

However,
first one requires 2 steps (add and multiply)
Second requires 3 steps ( multiply twice and add)


-}
double :: Int -> Int
double x = x + x

isZero 0 = True 
isZero n | n /= 0 = False

{-
Prefer to write non-overlapping functions

isZero 0 = True 
isZero n | n /= 0 = False

Non zero Int can be replaced with false

isZero :: Int -> Bool 
isZero 0 = True 
isZero n = False

isZero in the 2nd version cannot as n requires to not be zero

-}

------------------ Simple Examples ------------------

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]


------------------  Induction on Numbers ------------------

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

{-

> add Zero (Succ Zero)
Succ Zero


> add (Succ (Succ Zero)) (Succ ( Succ (Succ Zero)))

-> Succ add (Succ Zero) (Succ ( Succ (Succ Zero)))
-> Succ (Succ add Zero) (Succ ( Succ (Succ Zero)))

Succ (Succ (Succ (Succ (Succ Zero))))



-}



{-

-}

{-

-}

{-

-}



------------------  ------------------
























































