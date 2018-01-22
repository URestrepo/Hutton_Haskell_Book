{-
6.8 Exercises

1. How does the recursive version of the factorial function behave 
if applied to a negative argument, such as (-1)? 
Modify the definition to prohibit negative arguments 
by adding a guard to the recursive case.

-}
-- Revised Function has the error function in a guard
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n | n < 0 = error "input is negative"
      | otherwise = fib (n-2) + fib (n-1)


{-
2. Define a recursive function sumdown :: Int -> Int 
that returns the sum of the non-negative integers 
from a given value down to zero. 
For example, 
sumdown 3 
should return the result 
3+2+1+0 = 6

-}
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)


{-
3. Define the exponentiation operator ^ for non-negative integers 
using the same pattern of recursion as the multiplication operator *, 
and show how the expression 2 ^ 3 is evaluated using your definition.

(*&) :: Int -> Int -> Int
m *& 0 = 0
m *& n = m + (m *& (n-1))

Step 1: define the type
Step 2: enumerate the cases
Step 3: define the simple cases
Step 4: define the other cases
Step 5: generalize and simplify
-}
-- Note that ^* was used instead of ^.
-- For some reason, I was not able to get ^ to load in properly
-- Error: Ambiguous occurrence ‘^’. It could refer to either ‘Prelude.^’,

(^*) :: Int -> Int -> Int
m ^* 0 = 1
m ^* n = m * (m ^* (n-1))

{-
Steps

2^*3
= { applying ^* }
2 * 2^*2
= { applying ^* }
2 * 2 * 2^*1
= { applying ^* }
2 * 2 * 2 * 2^*0
= { applying ^* }
2 * 2 * 2 * 1
{ applying ^* }
8 

-}

{-
4. Define a recursive function euclid :: Int -> Int -> Int 
that implements Euclid’s algorithm for calculating the greatest 
common divisor of two non-negative integers: 
if the two numbers are equal, this number is the result; 
otherwise, the smaller number is subtracted from the larger, 
and the same process is then repeated. 
For example:

> euclid 6 27 
3


Step 1: define the type
Step 2: enumerate the cases
Step 3: define the simple cases
Step 4: define the other cases
Step 5: generalize and simplify
-}

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid y (x - y)
           | otherwise = euclid x (y - x)


{-
5. Using the recursive definitions given in this chapter, 
show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.


length :: [a] -> Int
length [] = 0 
length (_:xs) = 1 + length xs

length [1,2,3]
= { applying length }
 1 + length [2,3]
= { applying length }
1 + 1 + length [3]
= { applying length }
1 + 1 + 1 + length[]
= { applying length }
1 + 1 + 1 + 0
= { applying + }
3

drop :: Int -> [a] -> [a]
drop 0 xs      = xs
drop _ []      = []
drop n (_:xs)  = drop (n-1) xs

drop 3 [1,2,3,4,5]
= { applying drop }
drop 2 [2,3,4,5]
= { applying drop }
drop 1 [3,4,5]
= { applying drop }
drop 0 [3,4,5]
= { applying drop }
[3,4,5]

init :: [a] -> [a]
init [_] = [] 
init (x:xs) = x : init xs

init [1,2,3]
= { applying init }
1 : init [2,3]
= { applying init }
1 : 2: init [3]
= { applying init }
1 : 2 : [] 
= { applying Cons }
[1,2]

-}

{-
6. Without looking at the definitions from the standard prelude, 
define the following library functions on lists using recursion.

a. Decide if all logical values in a list are True:
and :: [Bool] -> Bool

b. Concatenate a list of lists:
concat :: [[a]] -> [a]


c. Produce a list with n identical elements:
replicate :: Int -> a -> [a]


d. Select the nth element of a list:
(!!) :: [a] -> Int -> a

e. Decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool

Note: most of these functions are defined in the prelude 
using other library functions rather than using explicit recursion, 
and are generic functions rather than being specific to the type of lists.

-}
and :: [Bool] -> Bool

{-
7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that
merges two sorted lists to give a single sorted list. For example:

> merge [2,5,6] [1,3,4] 
[1,2,3,4,5,6]

Note: your definition should not use other functions 
on sorted lists such as insert or isort, 
but should be defined using explicit recursion.
-}

{-
8. Using merge, define a function msort :: Ord a => [a] -> [a] that

implements merge sort, in which the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.

Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by at most one.
-}



{-
9. Using the five-step process, construct the library functions that:

a. calculate the sum of a list of numbers;

b. take a given number of elements from the start of a list;

c. select the last element of a non-empty list.
-}







