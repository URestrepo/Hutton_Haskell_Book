import Data.Char
{-
5.7 Exercises
1. Using a list comprehension, give an expression that calculates 
the sum 1^2 + 2^2 + ... 100^2 of the first one hundred integer squares.

Answer
> sum [x^2 | x <- [1..100]]

338350


2. Suppose that a coordinate grid of size m × n is given by the list of all pairs 
(x, y) of integers such that 
0 <= x <= m and
0 <= y <= n
Using a list comprehension, define a function grid :: Int -> Int -> [(Int,Int)] 
that returns a coordinate grid of a given size. For example:

> grid 1 2 
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]


-}
grid :: Int -> Int -> [(Int,Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]


{-
3. Using a list comprehension and the function grid above, 
define a function square :: Int -> [(Int,Int)] that returns 
a coordinate square of size n, excluding the diagonal from (0, 0) to (n, n). 

For example:
> square 2 
[(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
-}

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

{-
4. In a similar way to the function length, 
show how the library function replicate :: Int -> a -> [a] 
that produces a list of identical
elements can be defined using a list comprehension. 

For example:
> replicate 3 True 
[True,True,True]

-}
replicate' :: Int -> a -> [a]
replicate' a b = [b | _ <- [1..a]]

{-
5. A triple (x, y, z) of positive integers is Pythagorean 
if it satisfies the equation x^2 + y^2 = z^2 . 
Using a list comprehension with three generators, 
define a function pyths :: Int -> [(Int,Int,Int)] that returns 
the list of all such triples whose components are at most a given limit. For example:

> pyths 10 
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

-}
pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2 + b^2 == c^2]


{-
6. A positive integer is perfect if it equals 
the sum of all of its factors, excluding the number itself. 
Using a list comprehension and the function factors, 
define a function perfects :: Int -> [Int] that returns 
the list of all perfect numbers up to a given limit. 

For example:
> perfects 500 
[6,28,496]
-}

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> Bool
perfect x | sum (init $ factors x) == x = True
          | otherwise = False

perfects :: Int -> [Int]
perfects x = [a | a <- [1..x], perfect a]

{-
7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]]
with two generators can be re-expressed using two comprehensions with single generators. 
Hint: nest one comprehension within the other and make use of the library function 
concat :: [[a]] -> [a].

> [(x,y) | x <- [1,2], y <- [3,4]]
[(1,3),(1,4),(2,3),(2,4)]


Answer
concat [[(x,y) | x <- [1,2]] | y <- [3,4]]
> 

8. Redefine the function positions using the function find.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

> positions False [True, False, True, False] 
[1,3]

find' :: Eq a => a -> [(a,b)] -> [b]
find' k ts = [v | (k',v) <- ts, k == k']

For example:
> find ’b’ [(’a’,1),(’b’,2),(’c’,3),(’b’,4)] 
[2,4]


-}
find :: Eq a => a -> [(a,b)] -> [b]
find k ts = [v | (k',v) <- ts, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x [(a,b) | (a,b) <- zip xs [0..]]



{-

The scalar product of two lists of integers xs and ys of length n is given 
by the sum of the products of corresponding integers:

...

In a similar manner to chisqr, show how a list comprehension can be used 
to define a function scalarproduct :: [Int] -> [Int] -> Int
that returns the scalar product of two lists. For example:

> scalarproduct [1,2,3] [4,5,6] 
32

-}

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [a*b | (a,b) <- zip xs ys]


{-
10. Modify the Caesar cipher program to also handle upper-case letters.

-}
find' :: Eq a => a -> [(a,b)] -> [b]
find' k ts = [v | (k',v) <- ts, k == k']

pairs' :: [a] -> [(a,a)]
pairs' xs = zip xs (tail xs) 

sorted' :: Ord a => [a] -> Bool
sorted' xs = and [x <= y | (x,y) <- pairs' xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

uppers :: String -> Int
uppers xs = length [x | x <- xs, x >= 'A' && x <= 'Z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- let2int :: Char -> Int
-- let2int c = ord c - ord 'a'

-- int2let :: Int -> Char
-- int2let n = chr (ord 'a' + n)

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | otherwise = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2letUpper :: Int -> Char
int2letUpper n = chr (ord 'A' + n)

-- int2let :: Int -> Char
-- int2let n | n < 0     = chr (ord 'A' + n) 
--           | otherwise = chr (ord 'a' + n)


shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2letUpper ((let2int c + n) `mod` 26)
          | otherwise = c 

-- shift :: Int -> Char -> Char
-- shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
--           | isUpper c = int2let (((let2int c + n) `mod` 26) - 32)
--           | otherwise = c 

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

{-
The only thing that was edited is the ability to use encode with UpperCase letters
So
> encode (3) "haskell is FUN"
"kdvnhoo lv IXQ"

encode (-3) "kdvnhoo lv IXQ"
"haskell is FUN"

-}


{-
table :: [Float] 
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,0.2, 0.8, 
         4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 
         2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
             where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]


rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
   where
      factor = head (positions (minimum chitab) chitab)
      chitab = [chisqr (rotate n table') table | n <- [0..25]]
      table' = freqs xs



-}






