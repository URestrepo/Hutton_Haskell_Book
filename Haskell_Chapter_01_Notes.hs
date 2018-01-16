-- Haskell
--Chapter 1
-- QSORT function

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                  smaller = [a | a <- xs, a <= x]
                  larger = [b | b <- xs, b > x]
--sum prime function as Haskell has a sum function already
sum' [] = 0
sum' (n:ns) = n + sum' ns
seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)
-- 1.7 Exercises
--1. Give another possible calculation of the result double (double 2)
-- double 2 + double 2
-- (2+2) + (2+2)
-- 4 + (2+2)
-- 4 + 4
-- 8
--
--2. Show that sum [x] = x 
-- sum [x] = x + sum[]
--  -> x + 0
-- -> x
-- so sum [x] = x
--3. define a function product that takes in a list of numbers and show using your definition
product' [] = 1
product' (n:ns) = n * product' ns
-- product [2,3,4]
-- 
--24
--4. How should def of qsort be modified to produce reverse sorted version of list
qsortreverse [] = []
qsortreverse (x:xs) = qsortreverse larger ++ [x] ++ qsortreverse smaller
                      where
                        larger = [a | a <- xs, a >= x]
                        smaller = [b | b <- xs, b < x]
--5. what would be the effect of replacing <= by < in the original qsort? 
--Hint: consider qsort [2,2,3,1,1]
--Answer will exclude repetition
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
               where
                  smaller = [a | a <- xs, a < x]
                  larger = [b | b <- xs, b > x]
--qsort' [2,2,3,1,1]
--[1,2,3]