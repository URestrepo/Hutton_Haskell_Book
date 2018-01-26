import Data.List

{-
7.9 Exercises

1. Show how the list comprehension [f x | x <- xs,p x] can be 
re-expressed using the higher-order functions map and filter.



-}
map_filter :: (a -> a) -> (a -> Bool) -> [a] -> [a]
map_filter f p xs = map f (filter p xs) 

{-

> map_filter (+3) even [1..100]
[5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,
39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,
71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,
103]

> [(+3) x | x <- [1..100], even x]
[5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,
39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,
71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,
103]

-}

{-
2. Without looking at the definitions from the standard prelude, 
define the following higher-order library functions on lists.

Note: in the prelude the first two of these functions 
are generic functions rather than being specific to the type of lists.
-}

--a. Decide if all elements of a list satisfy a predicate:
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and $ map p xs

-- ******* recall that $ means ()

{-

-}

-- b. Decide if any element of a list satisfies a predicate:

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or $ map p xs 

-- c. Select elements from a list while they satisfy a predicate:



takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

-- d. Remove elements from a list while they satisfy a predicate:

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x =  dropWhile' p xs
                    | otherwise = x:xs



{-
3. Redefine the functions map f and filter p using foldr.

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v 
foldr' f v (x:xs) = f x (foldr' f v xs)

-}

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x) : xs) [] 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs) = foldr (\x xs -> if p x then x : xs else xs) [] (x:xs)

{-
4. Using foldl, define a function 
dec2int :: [Int] -> Int 
that converts a

decimal number into an integer. For example:

> dec2int [2,3,4,5] 
2345
-}
dec2int :: [Int] -> Int 
dec2int (x:xs) = foldl ((+) . (*10)) 0 (x:xs)



{-
5. Without looking at the definitions from the standard prelude, 
define the higher-order library function curry that converts 
a function on pairs into a curried function, and, conversely, 
the function uncurry that converts a curried function with two arguments
 into a function on pairs.

Hint: first write down the types of the two functions.

Curry assembles parameters into a tuple to be used by a function that uses a tuple.

-}

-- curry' :: ((a, b) -> c) -> a -> b -> c
-- curry' = (\# -> (\x -> (\y -> x # y)))

add :: (Int, Int) -> Int
add (x, y) = x + y


curry'' :: ((a, b) -> c) -> a -> b -> c
curry'' f = \x y -> f (x, y)

curry''' :: ((a, b) -> c) -> a -> b -> c
curry''' f m n = (\x y -> f (x, y)) m n

-- uncurry' :: a -> b -> ((a, b) -> c) -> c
-- uncurry' x y f = \x y ->

uncurry'' :: (a -> b -> c) -> (a,b) -> c
uncurry'' f = \(x, y) -> f x y 

{-
6. A higher-order function unfold that encapsulates a simple pattern of recursion
 for producing a list can be defined as follows:

 unfold p h t x | p x       = []
                | otherwise = h x : unfold p h t (t x)

That is, the function unfold p h t produces the empty list 
if the predicate p is true of the argument value, 
and otherwise produces a nonempty list by applying the 
function h to this value to give the head, and the function t 
to generate another argument that is recursively processed 
in the same way to produce the tail of the list. 

For example, the function int2bin can be rewritten 
more compactly using unfold as follows:

int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)

Redefine the functions chop8, map f and iterate f using unfold.
-}

{-

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = [] 
chop8 bits = take 8 bits : chop8 (drop 8 bits)


-}


int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)


unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- First we create a type Bit

chop8 :: [Bit] -> [[Bit]]
chop8 [] = [] 
chop8 bits = take 8 bits : chop8 (drop 8 bits)

{-
> chop8 [1,1,1,1,1,1,1,1,1,1,1,1,1,1]
[[1,1,1,1,1,1,1,1],[1,1,1,1,1,1]]
-}

type Bit = Int

chop8' :: [Bit] -> [[Bit]]
chop8' xs = unfold null (take 8) (drop 8) xs


map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

map''' :: (a -> b) -> [a] -> [b]
map''' f = unfold null (f . head) tail


-- iterate' f x = [x, f x, f (f x), f (f (f x)), ...]

iterate' :: (a -> a) -> a -> [a]
iterate' f x = unfold (const False) id f x
-- iterate' f x = unfold (const False x) id f x


{-
Modify the binary string transmitter example to detect simple transmission errors 
using the concept of parity bits. That is, each eight-bit binary number produced 
during encoding is extended with a parity bit, set to one if the number contains 
an odd number of ones, and to zero otherwise. In turn, each resulting nine-bit 
binary number consumed during decoding is checked to ensure that its parity bit 
is correct, with the parity bit being discarded if this is the case, and a parity 
error being reported otherwise.

Hint: the library function error :: String -> a displays the 
given string as an error message and terminates the program; 
the polymorphic result type ensures that error can be used in any context.

-}








