-- Haskell
--Chapter 4
-- Defining Functions

{-
------------------ New from Old ------------------
- Easy way to define functions using old functions

Ex
-}

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

recip' :: Fractional a => a -> a
recip' n = 1/n

{-
------------------ Conditional Expressions ------------------
Expressions that use logical conditions
ex
if true choose x
if false choose y

-}

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n


{-
Conditionals may be nested
-}

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
                   if n == 0 then 0 else 1

{-
Conditionals in Haskell must have else

------------------ Guarded Equations ------------------
 Alternative to conditional expressions

* Always remember to write the Type Definitions

-}


abs'' :: Int -> Int
abs'' n | n >= 0    = n
        | otherwise = -n


signum'' :: Int -> Int
signum'' n | n < 0     = -1
           | n == 0    = 0
           | otherwise = 1

{-
------------------ Pattern Matching ------------------



One can also use wildcard operators to take place of inputs

One can use lazy evaluation to make assumptions for additional arguments as in example 4

-}

not' :: Bool -> Bool
not' False = True
not' True  = False


(&&-) :: Bool -> Bool -> Bool
True &&- True   = True 
True &&- False  = False
False &&- True  = False
False &&- False = False

(&&*) :: Bool -> Bool -> Bool
True &&* True = True
_    &&* _    = False

(&&**) :: Bool -> Bool -> Bool
True &&** b  = b
False &&** _ = False

{-
------------------ Tuple Patterns ------------------

One can use wildcard in tuple definitions as it may not matter for functions


-}

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y



{-
------------------ List Patterns ------------------

one can use patterns in the function definitions to check outputs
-}

test' :: [Char] -> Bool
test' ['a',_,_] = True
test' _         = False

{-

* Know that lists are composed from an empty list and constructed to contain whatever
is inside the list

example
[1,2,3]

= 1:2:3:[]



using the above principle, one can make generalized functions
for example, test' can check a list of any size and see if first value is the value you seek
-}

test'' :: [Char] -> Bool
test'' ('a':_) = True
test'' _       = False

-- Other examples

head' :: [a] -> a
head' (x:_) = x


tail' :: [a] -> [a]
tail' (_:xs) = xs


{-
------------------Lambda Expressions------------------
Functions without names
May need to make functions on the fly

ex
(\x -> x + x) 2

* must paste into repl as having in the script and loading script does not work


add :: Int -> Int -> Int 
add x y = x + y

can be understood as meaning

add :: Int -> (Int -> Int) 
add = \x -> (\y -> x + y)
-}

add :: Int -> (Int -> Int) 
add = \x -> (\y -> x + y)


const' :: a -> (b -> a) 
const' x = \_ -> x

odds' :: Int -> [Int]
odds' n = map f [0..n-1]
            where f x = x*2 + 1

odds'' :: Int -> [Int]
odds'' n = map (\x -> x*2 + 1) [0..n-1] 


{-

------------------ Operator Sections ------------------
Functions that are operators have sections
sections serve as arguments for a function, kinda serve as placeholder

Ex
(x) = \x -> (\y -> x # y)
(x #) = \y -> x # y
(# y) = \x -> x # y


(+) is the addition function \x -> (\y -> x+y) 

(1+) is the successor function \y -> 1+y

(1/) is the reciprocation function \y -> 1/y 

(*2) is the doubling function \x -> x*2

(/2) is the halving function \x -> x/2
-}

































