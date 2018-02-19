-- Haskell
--Chapter 15
-- Lazy Evaluation


{-
Review concept of evaluation, 
consider evaluation strategies and their properties, 
discuss infinite structures and modular programming,
and conclude with special form of function application to improve space performance of programs


-}

------------------ Introduction ------------------

inc :: Int -> Int
inc n = n + 1

{-
inc (2*3) 
=   { applying * } 
inc 6
= { applying inc }
6 + 1 
= { applying + } 
7

or 

inc (2*3) 
{ applying inc } 
(2*3) + 1
{ applying * }
6 + 1
{ applying + }
7


-}

------------------ Evaluation Strategies ------------------

{-

Reducible Expression or redex
- expression that has a function applied to to or more arguments

-}

mult :: (Int,Int) -> Int
mult (x,y) = x * y

{-
innermost evaluation
- Redex the innermost (if two, leftmost)

mult (1+2, 2+3) 
{ applying the first + } 
mult (3, 2+3)
{ applying + }
mult (3, 5) 
{ applying mult } 
3 * 5
{ applying * }
15

-- Allows arguments to be evaluated before functions applied. Allowing "Pass by Value"
-- Also forces leftmost innermost evaluation
-}

{-
Outermost Evaluational
- Redux the outermost (if two, leftmost)

mult (1+2, 2+3) 
{ applying mult } 
(1+2) * (2+3)
{ applying the first + }
3 * (2+3) 
{ applying + }
3 * 5
{ applying * }
15

- Allows functions to be applied before arguments are evaluated
  - in this case arguments are "Passed by Name"
- Note that certain functions require arguments to be evaluated first
  - As in above, * and + require arguments first to be evaluated
    - This property is called "strict"
-}

-- Lambda Expressions

-- Curried Mult
mult' :: Int -> Int -> Int
mult' x = \y -> x * y

{-
Innermost Evaluation

mult (1+2) (2+3) 
{ applying the first + } 
mult 3 (2+3)
{ applying mult }
(\y -> 3 * y) (2+3) 
{ applying + } 
(\y -> 3 * y) 5
{ applying the lambda }
3 * 5 
{ applying * } 
15

Reduxing lambdas prohibited as lambdas are viewed as black boxes that cannot be seen inside.
Reduction then happens within lambda body happens once function has been applied.

-}

------------------ Termination ------------------

inf :: Int
inf = 1 + inf

{-
The above is a successor function of infinity that will never terminate

Now using fst on innermost call-by-value method

fst (0, inf) 
{ applying inf } 
fst (0, 1 + inf)
{ applying inf } 
fst (0, 1 + (1 + inf)) 
{ applying inf }
fst (0, 1 + (1 + (1 + inf))) 
{ applying inf }
...

function will never terminate

On other hand, call by name with the result 0 will be in 1 step by immediately applying fst


fst (0, inf) 
{ applying fst } 
0


*******
if there exists any evaluation sequence that terminates for a given expression, 
then call-by-name evaluation will also terminate for this expression, 
and produce the same final result.

call-by-name evaluation is preferable to call-by-value for the purpose of ensuring that evaluation terminates as often as possible.

-}

------------------ Number of Reductions ------------------

square :: Int -> Int
square n = n * n

{-
Call-By-Value
square (1+2) 
{ applying + } 
square 3
{ applying square }
3 * 3 
{ applying * } 
9

Call-By-Name

square (1+2) 
{ applying square } 
(1+2) * (1+2)
{ applying the first + }
3 * (1+2) 
{ applying + } 
3 * 3
{ applying * }
9


***Arguments are evaluated precisely once using call-by-value evaluation, 
but may be evaluated many times using call-by-name.


The use of call-by-name evaluation in conjunction with sharing is known as lazy evaluation
-}

------------------  Infinite Structures ------------------
ones :: [Int] 
ones = 1 : ones

{-
Call-by-Value 
head ones

will never terminate as it requires to evaluate 1:1:...

Call-by-Name will terminate

using lazy evaluation, expressions are only evaluated as much as required by the context in which they are used.

-}

------------------ Modular Programming ------------------

{-
Lazy evaluation allows one to separate control from data in computations

For example 

> take 3 ones
[1,1,1]
selecting first 3 elements (control) of list of infinite ones (data)


****
takeWhile  (boolean)

Useful function
****



-}

{-

Steps for infinite sequence of prime numbers

- write down the infinite sequence 2, 3, 4, 5, 6, ...; 
- mark the first number, p, in the sequence as prime;
- delete all multiples of p from the sequence;
- return to the second step.

-}
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

{-
> take 10 primes 
[2,3,5,7,11,13,17,19,23,29]

> takeWhile (< 10) primes 
[2,3,5,7]
-}

------------------ Strict Application ------------------


{-

Haskell uses lazy evaluation by default, but using $! one can force x in f(x) to be evaluated before
f is applied.

for basic types, nothing is done as it is already completely evaluated

for pair types (Int,Bool) eval is performed until we have pair

for lists, until cons empty list is obtained.

In regards to curried functions

(f $! x) y forces top-level evaluation of x 
(f x) $! y forces top-level evaluation of y 
(f $! x) $! y forces top-level evaluation of x and


-}

sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x:xs) = sumwith (v+x) xs

{-
above function will take uo too much space as it will create entire summation first before numbers are added

sumwith 0 [1,2,3] 
{ applying sumwith }
sumwith (0+1) [2,3] 
{ applying sumwith }
sumwith ((0+1)+2) [3] 
{ applying sumwith }
sumwith (((0+1)+2)+3) [] = 
{ applying sumwith } 
((0+1)+2)+3
{ applying the first + }
(1+2)+3
{ applying the first + }
3+3
{ applying + }
6


it is preferable to add (v+x) as soon as introduced.= to improve space.

-}

sumwith' :: Int -> [Int] -> Int
sumwith' v [] = v
sumwith' v (x:xs) = sumwith' (v+x) xs

{-

sumwith 0 [1,2,3] 
{ applying sumwith } 
(sumwith $! (0+1)) [2,3]
{ applying + } 
(sumwith $! 1) [2,3]
{ applying $! }
sumwith 1 [2,3] 
{ applying sumwith } 
(sumwith $! (1+2)) [3]
{ applying + } (sumwith $! 3) [3]
{ applying $! }
sumwith 3 [3] 
{ applying sumwith } 
(sumwith $! (3+3)) []
{ applying + }
(sumwith $! 6) [] 
{ applying $! } sumwith 6 []
{ applying sumwith }
6

-}
foldl’ :: (a -> b -> a) -> a -> [b] -> a 
foldl’ f v [] = v 
foldl’ f v (x:xs) = ((foldl’ f) $! (f v x)) xs


sumwith'' :: Int -> [Int] -> Int
sumwith'' = foldl' (+)

{-
> sumwith'' 0 [1,2,3]
6
-}

{-

-}



------------------  ------------------
























































