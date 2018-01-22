

-- Haskell
--Chapter 6
-- Recursive Functions

{-
------------------ Basic Concepts -----------------
Recursion is the basic mechanism for looping in Haskell.

As one knows, many functions can be defined by other functions
Ex Factorial 
-}
fac' :: Int -> Int
fac' n = product [1..n]

-- However functions can be defined in terms of themselves
-- This is called Recursion. Functions then are recursive.

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1) 


{-
Understand that fac 0 = 1 refers to what is called the base case.
fac' n = n * fac (n-1) then states that the factorial of any number is the product of that number
and the factorial of its predecessor. This is called Recursive Case.



-}

(*&) :: Int -> Int -> Int
m *& 0 = 0
m *& n = m + (m *& (n-1))

{-
Ex from above.
4 * 3 
= { applying * } 
4 + (4 * 2)
= { applying * }
4 + (4 + (4 * 1)) 
{ applying * } 
4 + (4 + (4 + (4 * 0)))
= { applying * }
4 + (4 + (4 + 0)) 
{ applying + } 
12
-}


------------------ Recursion on Lists ------------------

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

{-


product' [2,3,4]
= { applying product' }
2 * product [3,4]
= { applying product' }
2 * (3 * product [4])
= { applying product' }
2 * (3 * (4 * product' []))
= { applying product' }
2 * (3 * (4 * 1))
= { applying * }
24
-}

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- The use of wildcard above only describes that the value of head is irrelevant to calculate length

-- Reversing a list can also be written recursively

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

{-


reverse' [1,2,3] 

= { applying reverse' } 
reverse' [2,3] ++ [1]

= { applying reverse' } 
(reverse' [3] ++ [2]) ++ [1]

= { applying reverse' } 
((reverse' [] ++ [3]) ++ [2]) ++ [1] 

= { applying reverse' }
(([] ++ [3]) ++ [2]) ++ [1] 

= { applying ++ } 
[3,2,1]



One can use recursion for multiple arguments. A good example of this is drop 
-}


drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

-- Notice the two base cases above (dropping 0 elements and an empty list)
-- I think the _ can be replaced with x, but I think it is good practice that if
-- the element of thing in question is irrelevant, then use the wildcard "_"
 

------------------ Multiple Recursion ------------------
{-
Multiple Recursion is when function is applied more than once using its own definition

Example Fibonacci
0,1,1,2,3,5,8,13

Uses in a sense two base cases, when first number is 0 and second number is 1


-}
-- In case, this function simply calculates the nth position of a Fibonacci sequence
-- Hence it takes in an Int and outputs an Int

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- Quicksort is another such function that uses multiple recursion

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where 
                    smaller = [a | a <- xs, a <= x] 
                    larger  = [b | b <- xs, b > x]


------------------ Mutual Recursion ------------------
{-
An odd but useful form of recursion such that two or more functions are defined recursively in
terms of one another


Look at the trickery of using odd and even
-}

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)


odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

{-

Amazingly, the above equations take advantage of the intrinsic of even and odd numbers
If asking if 4 is odd, the recurive function will stop at odd' 0 meaning it will be false

Ex
Is 4 even, let us find out

even 4 
= { applying even } 
odd 3
= { applying odd } 
even 2 
= { applying even }
odd 1 
= { applying odd } 
even 0
= { applying even }
True


This playing of which function a recursive function lands on can be seen also in below functions

-}

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs
-- Recall that the : symbol is the construct symbol used to construct lists

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

-- Recall that if a function does not need a variable, it uses to the wild-card "_" as a symbol
-- to mean disregard. This can be seen in odds

{-
evens "abcde" 
= { applying evens } 

’a’ : odds "bcde"
= { applying odds } 

’a’ : evens "cde" 
= { applying evens }

’a’ : ’c’ : odds "de" 
= { applying odds } 


’a’ : ’c’ : evens "e"
= { applying evens } 

’a’ : ’c’ : ’e’ : odds [] 
= { applying odds }

’a’ : ’c’ : ’e’ : [] 
{ string notation } 
"ace"

a is position 0
c is position 2
e is position 4

-} 
------------------ Advice on Recursion ------------------
{-
Recursion is difficult but becomes more natural with practice

These steps should be useful
Step 1: define the type
Step 2: enumerate the cases
Step 3: define the simple cases
Step 4: define the other cases
Step 5: generalize and simplify

Step 1: define the type
- Always define the types that should be taken in and outputted

Step 2: enumerate the cases
- Consider the standard cases 
- For example is there a case for use of an empty list

Step 3: define the simple cases
- What is the base case or smallest version that needs to be considered for all versions of function

Step 4: define the other cases
- What do we do for all other cases?
- it is useful to first consider the ingredients that can be used, 
- such as the function itself , the arguments , and perhaps library functions (+, -, *, and so on.)

Step 5: generalize and simplify
- The function is basically done now. But at this point it may necessary to generalize
- or perhaps simplify.
- Ex, does the function only apply to numbers. Must items be order-able?

Example for Product Function
Step 1: define the type

product :: [Int] -> Int

Step 2: enumerate the cases

product []     = 
product (n:ns) =

Step 3: define the simple cases

product [] = 1
product (n:ns) =

Step 4: define the other cases
product [] = 1 
product (n:ns) = n * product ns

Step 5: generalize and simplify

Generalize
product :: Num a => [a] -> a

Simplify
product = foldr (*) 1

so that final function is

product :: Num a => [a] -> a 
product = foldr (*) 1




-}





















