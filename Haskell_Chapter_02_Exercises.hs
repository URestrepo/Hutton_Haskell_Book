-- Haskell
--Chapter 2.7 Exercises

{-
1. Work through the examples from this chapter using GHCi.
In Notes

2. Parenthesise the following numeric expressions:

2^3*4 
(2^3)*4 


2*3+4*5 

(2*3)+(4*5) 

2+3*4^5

2+(3*(4^5))


3. The script below contains three syntactic errors. Correct these errors and then check that your script works properly using GHCi.

bad
N = a ’div’ length xs 
    where
        a = 10 
        xs = [1,2,3,4,5]


should work
-}
n = a `div` length xs 
    where
        a = 10 
        xs = [1,2,3,4,5]

{-
4. The library function last selects the last element of a non-empty list; for example, last [1,2,3,4,5] = 5. Show how the function last could be defined in terms of the other library functions introduced in this chapter. Can you think of another possible definition?

Solution 1
-}
last' ns = head (reverse ns)

--Solution 2
last'' ns = head (drop (length ns-1) ns)

--Solution 3
last''' ns = ns !! (length ns-1)

{-
5. The library function init removes the last element from a non-empty list; 
for example, init [1,2,3,4,5] = [1,2,3,4]. 
Show how init could similarly be defined in two different ways.

-}

--Solution 1
init' ns = reverse (drop 1 (reverse ns))


--Solution 2

init'' ns = take (length ns-1) ns

--Solution 3
init''' ns =  reverse (tail (reverse ns))

















