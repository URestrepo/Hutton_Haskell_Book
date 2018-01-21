import Data.Char

-- Haskell
--Chapter 5
-- List Comprehensions

{-
-
Things that should be explained
- Zip
- Generators
- Guards
- other stuff

------------------ Basic Concepts -----------------
Comprehension
Construct lists from lists similar to the math comprehension that creates sets

Examples that can be pasted in repl


> [X^2 | x <- [1..5]]
[1,4,9,16,25]

The symbol | is read as such that, <- is read as is drawn from, 
and the expression x <- [1..5] is called a generator

> [(x,y) | x <- [1,2,3], y <- [4,5]]
[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

> [(x,y) | y <- [4,5], x <- [1,2,3]]
[(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

* Generators are nested, so in above 2 examples, one can see which differences in which part changes more


Generators can also depend on values of variables from earlier generators
Ex
> [(x,y) | x <- [1..3], y <- [x..3]]
[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

-}

-- Function to concatenate a list of lists that uses one generator to select each list in turn
--  then select each element from each list
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- concat' [[1,2,3],[4,5,6]]
-- [1,2,3,4,5,6]

-- Wildcards can also be used
--  For example to collect the first of elements and discard rest

firsts' :: [(a,b)] -> [a]
firsts' ps = [x | (x,_) <- ps]

{-
firsts' [(1,2),(3,4),(5,6)]
[1,3,5]

-}


length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]


{-

------------------ Guards ------------------
Guards are logical expressions that can be used to filter values

For example a comprehension for even numbers

> [x | x <- [1..10], even x]
[2,4,6,8,10]


* A guard must output a boolean

-}

factors' :: Int -> [Int]
factors' n = [x | x <- [1..n], n `mod` x == 0]

prime' :: Int -> Bool
prime' n = factors' n == [1,n]

-- Know that Haskell uses lazy evaluation to say a number is not prime when it detects any factor that is not itself and 1

-- Prime Function
primes' :: Int -> [Int]
primes' n = [x | x <- [2..n], prime' x]


-- Eq a => says that a belongs to the Eq class
find' :: Eq a => a -> [(a,b)] -> [b]
find' k ts = [v | (k',v) <- ts, k == k']

-- find' 'b' [('a',1),('b',2),('c',3),('b',4)]
-- [2,4]

------------------ Zip Function ------------------
{-

zip combines pairs of two lists until any list is exhausted
ex
> zip ['a','b','c'] [1,2,3,4] 
[('a',1),('b',2),('c',3)]

-}
pairs' :: [a] -> [(a,a)]
pairs' xs = zip xs (tail xs) 

-- One can check if pairs are ordered using class of Ord

sorted' :: Ord a => [a] -> Bool
sorted' xs = and [x <= y | (x,y) <- pairs' xs]


-- and puts in "and" between all values of a list and outputs a boolean
-- Understand that "x <= y" returns a boolean for the list comprehension

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

{-
positions False [True, False, True, False] 
[1,3]
-}

------------------ String Comprehensions ------------------
{-

One can use list functions on strings
Ex

> "abcde" !! 2
'c'

> take 3 "abcde"
"abc"

> length "abcde"
5

> zip "abc" [1,2,3,4]
[('a',1),('b',2),('c',3)]

-}

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

{-
> lowers "Haskell"
6

> count 's' "Mississippi"
4

-}

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c 


-- We will not use the almighty list comprehension

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


{-
> encode 3 "haskell is fun" 
"kdvnhoo lv ixq"

> encode (-3) "kdvnhoo lv ixq" 
"haskell is fun"

-}

table :: [Float] 
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,0.2, 0.8, 
         4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 
         2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

{-
> percent 5 15
33.333336

-}

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
             where n = lowers xs

{-
> freqs "abbcccddddeeeee"
[6.666667,13.333334,20.0,26.666668,33.333336,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0
.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

-}


chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]


rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

{-
> rotate 3 [1,2,3,4,5] 
[4,5,1,2,3]


> let table' = freqs "kdvnhoo lv ixq"

> [chisqr (rotate n table') table | n <- [0..25]]
[1408.8524,640.0218,612.3969,202.42024,1439.9456,4247.318,
650.9992,1164.7708,972.1826,993.1813,497.46844,1488.8606,
2296.3413,1407.4161,1491.524,3033.984,659.5394,2836.3345,
984.7049,809.6876,1310.4423,850.64154,2908.0313,954.4321,5313.5776,626.4024]

-}

crack :: String -> String
crack xs = encode (-factor) xs
   where
      factor = head (positions (minimum chitab) chitab)
      chitab = [chisqr (rotate n table') table | n <- [0..25]]
      table' = freqs xs


{-
> crack "kdvnhoo lv ixq"
"haskell is fun"

> crack "vscd mywzboroxcsyxc kbo ecopev"
"list comprehensions are useful"

> crack (encode 3 "haskell")
"piasmtt"


> crack (encode 3 "boxing wizards jump quickly") 
"wjsdib rduvmyn ephk lpdxfgt
-}























