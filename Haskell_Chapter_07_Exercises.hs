import Data.List
import Data.Char

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




unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- First we create a type Bit

-- chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = [] 
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)

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


int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bin2int :: [Bit] -> Int 
bin2int bits = sum [w*b | (w,b) <- zip weights bits] 
                     where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int 
bin2int' = foldr (\x y -> x + 2*y) 0

make8 :: [Bit] -> [Bit] 
make8 bits = take 8 (bits ++ repeat 0)

-- encode :: String -> [Bit]
-- encode = concat . map (make8 . int2bin . ord)

encode :: String -> [Bit]
encode = concat . map (make_parity . make8 . int2bin . ord)

make_parity :: [Bit] -> [Bit]
make_parity bits | (even . sum) bits = bits ++ [0]
                 | otherwise = bits ++ [1]

check_parity :: [Bit] -> Bool
check_parity bits | bytecode_even == parity_value = True
                  | otherwise = False
                  where bytecode_even = (even . sum . take 8) bits
                        parity_value = (even . last) bits

chop8 :: [Bit] -> [[Bit]]
chop8 [] = [] 
chop8 bits | check_parity bits == False = error "Parity Error"
           | otherwise = take 8 bits : chop8 (drop 9 bits)

-- decode :: [Bit] -> String 
-- decode = map (chr . bin2int) . chop8

decode :: [Bit] -> String 
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit] 
channel = id

transmit :: String -> String 
transmit = decode . channel . encode


{-

8. Test your new string transmitter program from the previous exercise 
using a faulty communication channel that forgets the first bit, which 
can be modelled using the tail function on lists of bits.

-}
noisy_channel :: [Bit] -> [Bit]
noisy_channel bits = tail bits

-- Why did (noisy_channel  = tail) not work?

noisy_transmit :: String -> String
noisy_transmit = decode . noisy_channel . encode

{-
*Main> noisy_transmit "to"
":*** Exception: Parity Error
CallStack (from HasCallStack):
  error, called at Haskell_Chapter_07_Exercises.hs:263:43 in main:Main
*Main> transmit "to"
"to"

-}

{-
9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
that alternately applies its two argument functions to successive elements in a list, 
in turn about order. 

For example:
> altMap (+10) (+100) [0,1,2,3,4] 
[10,101,12,103,14]
-}



altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x:y:xs) =  f x : g y : altMap f g xs

{-
Alternative solution using a comprehension.

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' f g xs = [if even d then f c else g c | (c, d) <- zip xs [0..]]
-}

{-
10. Using altMap, define a function luhn :: [Int] -> Bool that implements 
the Luhn algorithm from the exercises in chapter 4 for bank card numbers of any length. 
Test your new function using your own bank card.



-}

-- > luhn 1 7 8 4 True

-- > luhn 4 7 8 3 False

luhnDouble :: Int -> Int 
luhnDouble x | 2*x > 9   = 2*x - 9
             | otherwise = 2*x

luhn :: [Int] -> Bool
luhn ns = ((== 0) . (`mod` 10)) . sum . (altMap luhnDouble id) $ ns

-- luhn :: [Int] -> [Int]
-- luhn ns = (altMap luhnDouble id ns)

{- 
8. The Luhn algorithm is used to check bank card numbers 
for simple errors such as mistyping a digit, and proceeds as follows:
consider each digit as a separate number; moving left, 
double every other number from the second last; 
subtract 9 from each number that is now greater than 9; 
add all the resulting numbers together; 
if the total is divisible by 10, the card number is valid.

Define a function luhnDouble :: Int -> Int 
that doubles a digit and subtracts 9 if the result is greater than 9. For example:

> luhnDouble 3 6

> luhnDouble 6 3

Using luhnDouble and the integer remainder function mod, define a function 
luhn :: Int -> Int -> Int -> Int -> Bool that decides if a
four-digit bank card number is valid. For example:

> luhn 1 7 8 4 True

> luhn 4 7 8 3 False

In the exercises for chapter 7 we will consider a more general version of this 
function that accepts card numbers of any length.
-}

-- luhnDouble :: Int -> Int 
-- luhnDouble x | 2*x > 9   = 2*x - 9
--              | otherwise = 2*x


-- luhn' :: Int -> Int -> Int -> Int -> Bool
-- luhn' a b c d | (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0 = True
--               | otherwise                                           = False


-- -- To make code more organized, one can use the where (which allows you to define values)

-- luhn'' :: Int -> Int -> Int -> Int -> Bool
-- luhn'' a b c d | total `mod` 10 == 0 = True
--                | otherwise           = False
--                  where total = luhnDouble a 
--                              + b
--                              + luhnDouble c 
--                              + d





























