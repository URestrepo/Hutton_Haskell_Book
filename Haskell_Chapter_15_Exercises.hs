-- 15.9 Exercises
{-
1. Identify the redexes in the following expressions, and 
determine whether each redex is innermost, outermost, neither, or both:

1 + (2*3) 

Redux:
(2*3)

Innermost and Outermost

(1+2) * (2+3) 

Redux:
1+2
2+3

Innermost


fst (1+2, 2+3) 

Redux:
1+2
2+3
fst

Innermost, last Outermost


(\x -> 1 + x) (2*3)

Redux:
2*3
(\x -> 1 + x)

Innermost, then Outermost


-}


{-
2. Show why outermost evaluation is preferable to innermost for the purposes of evaluating the expression fst (1+2,2+3).

Outermost:

fst (1+2, 2+3) 
{ applying + }
fst (3, 2+3)  
{ applying fst } 
3

Innermost:
fst (1+2, 2+3) 
{ applying + }
fst (3, 2+3) 
{ applying + }
fst (3, 5)  
{ applying fst } 
3

Outermost evaluation is preferable because it avoids evaluating the second argument, 
and therefore takes one fewer reduction steps.

-}

{-
3. Given the definition mult = \x -> (\y -> x * y), 
show how the evaluation of mult 3 4 can be broken down into four separate steps.

mult 3 4
{ applying mult }
= \x -> (\y -> x * y) 3 4
{ applying the outer lambda }
= (\y -> 3 * y) 4
{ applying the lambda }
= 3 * 4
{ applying * }
= 12

-}

{-
4. Using a list comprehension, define an expression 
fibs :: [Integer] 
that generates the infinite sequence of Fibonacci numbers

0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...

using the following simple procedure:
1. the first two numbers are 0 and 1; 
2. the next is the sum of the previous two; 
return to the second step.

Hint: make use of the library functions zip and tail. 
Note that numbers in the Fibonacci sequence quickly become large, 
hence the use of the type Integer of arbitrary-precision integers above.

-}
fibs :: [Integer] 
fibs = 0 : 1 : [ x + y | (x,y) <- (zip (fibs) (tail fibs))]


{-
> take 10 fibs
[0,1,1,2,3,5,8,13,21,34]
-}

{-
5. Define appropriate versions of the library functions 

repeat :: a -> [a] 
repeat x = xs where xs = x:xs

take :: Int -> [a] -> [a] 
take 0 _ = [] 
take _ [] = [] 
take n (x:xs) = x : take (n-1) xs

replicate :: Int -> a -> [a] 
replicate n = take n . repeat

for the following type of binary trees:

data Tree a = Leaf | Node (Tree a) a (Tree a) 
            deriving Show
-}
data Tree a = Leaf | Node (Tree a) a (Tree a) 
            deriving Show

repeatTree :: a -> Tree a
repeatTree x = Node (repeatTree x) x (repeatTree x)

takeTree :: Int -> Tree a -> Tree a
takeTree 0 t            = Leaf
takeTree _ Leaf         = Leaf
takeTree n (Node l x r) = Node (takeTree (n-1) l ) x (takeTree (n-1) r )

replicateTree :: Int -> a -> Tree a
replicateTree n = takeTree n . repeatTree



{-



> takeTree 1 (Node (Node (Leaf) 1 (Leaf)) 2 (Node (Leaf) 3 (Leaf)))
Node Leaf 2 Leaf

> takeTree 3 (Node (Node (Leaf) 1 (Leaf)) 2 (Node (Leaf) 3 (Leaf)))
Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


> replicateTree 0 10
Leaf

n> replicateTree 1 10
Node Leaf 10 Leaf

> replicateTree 2 10
Node (Node Leaf 10 Leaf) 10 (Node Leaf 10 Leaf)

-}
               

{-
6. Newton’s method for computing the square root of a (non-negative) floating-point number n 
can be expressed as follows:

1. Start with an initial approximation to the result; 
2. given the current approximation a, 
the next approximation is defined by the function next a = (a + n/a) / 2; 
3. repeat the second step until the two most recent approximations are within some desired 
distance of one another, at which point the most recent value is returned as the result.

Define a function sqroot :: Double -> Double that implements this procedure. 
Hint: first produce an infinite list of approximations using the library function iterate. 
For simplicity, take the number 1.0 as the initial approximation, and 0.00001 as the distance value.

> :t iterate
iterate :: (a -> a) -> a -> [a]

> take 10 (iterate (2*) 1)
[1,2,4,8,16,32,64,128,256,512]

Under stand that 2* will be curried because it already has one side of multiplication


-}

closeTo' :: Double -> Double -> Bool
closeTo' x y = if abs( x - y) <= 0.00001 then True else False

next :: Double -> Double -> Double
next n a = (a + n/a) / 2

closeTo :: [Double] -> Double
closeTo (x:y:xs)  = if abs( x - y ) <= 0.00001 then x else closeTo (y:xs)


sqroot :: Double -> Double
sqroot n = closeTo (iterate (next n) 1)

{-


-}

{-
> closeTo' 5 6
False

> closeTo' 5.999999999 6
True

> sqroot 4
2.0000000929222947


Ask C if it would be better to write like he did with the .00001 in the earlier function?

-}















