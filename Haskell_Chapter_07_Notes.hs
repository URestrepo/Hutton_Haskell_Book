import Data.Char
import Data.List

-- Haskell
--Chapter 7
-- Higher-order functions

{-
------------------ Basic Concepts -----------------
Higher-order functions allow common programming patterns to be encapsulated as functions


For example
-}

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

{-
> twice' (reverse) [1,2,3] 
12

> twice' (reverse) [1,2,3] 
[1,2,3]



a function that takes a function as an argument 
or returns a function as a result is called a higher-order function

*Note because of Haskell's curried function, the first def does not apply


map is an example of a function
-}
map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs]

{-
(a -> b) refers to a function
****** How does one read (a -> b)? ******

[ f x | x <- xs] is a comprehension
function f is applied to x whereby x comes from list xs
which is then outputted as a list



map f xs returns the list of all values f x such that 
x is an element of the argument list xs

> map (+1) [1,3,5,7] 
[2,4,6,8]

> map even [1,2,3,4] 
[False,True,False,True] 

> map reverse ["abc","def","ghi"] 
["cba","fed","ihg"]


3 things to know of map

1. it is polymorphic function that can be applied to lists of type any
2. it can be applied to itself for nested lists
3. Number 2 should hint that it can be defined recursively

-}
map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x ]

-- Filter can also be recursive
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f []                 = []
filter'' f (x:xs) | f x       = x : filter'' f xs
                  | otherwise = filter'' f xs

{-
> filter'' even [1..10] 
[2,4,6,8,10]

> filter'' (> 5) [1..10] 
[6,7,8,9,10]

> filter'' (/= ' ') "abc def ghi" 
"abcdefghi"


Filter can sometimes be written with p meaning predicate
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p []                 = []
filter'' p (x:xs) | p x       = x : filter'' p xs
                  | otherwise = filter'' p xs

map and filter are many times used together

Ex
-}
sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))


{-
Useful higher order functions
Decide if all elements of a list satisfy a predicate:

> all even [2,4,6,8] 
True

Decide if any element of a list satisfies a predicate:

> any odd [2,4,6,8] 
False

Select elements from a list while they satisfy a predicate:

> takeWhile even [2,4,6,7,8] 
[2,4,6]

Remove elements from a list while they satisfy a predicate:

> dropWhile odd [1,3,5,6,7] 
[6,7]

-}

------------------ The foldr Function ------------------
{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v 
foldr f v (x:xs) = f x (foldr f v xs)

-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v 
foldr' f v (x:xs) = f x (foldr' f v xs)




add_5 :: Int -> Int
add_5 = (+ 5)

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = snoc x (reverse xs)

reverse'' :: [a] -> [a]
reverse'' xs = foldr snoc [] xs


------------------ fold_l ------------------

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f v [] = v 
foldl'' f v (x:xs) = foldl' f (f v x) xs




{-

fold r
f [] = v 
f (x:xs) = x # f xs

fold r
f v [] = v 
f v (x:xs) = f (v # x) xs


difference being that 
fold r evaluates from right to left
fold l evaluates the expression v # x first then from left to right



-}


------------------ Composition Operator ------------------

(.*) :: (b -> c) -> (a -> b) -> (a -> c) 
f .* g = \x -> f (g x)


{-

(.) :: (b -> c) -> (a -> b) -> (a -> c) 
f . g = \x -> f (g x)


f . g, which is read as f composed with g, is the function that takes an

argument x, applies the function g to this argument, and applies the function f to the result.



-}

odd' n = not (even n) 


twice'' :: (t -> t) -> t -> t 
twice'' f x = f (f x)

sumsqreven' ns = sum (map (^2) (filter even ns))



-- The above functions can be written more clearly

odd'' = not . even

twice''' f = f .f

sumsqreven'' = sum . map (^2) . filter even

-- for clarity, though not necessary because of the functions are associative

sumsqreven''' = sum . (map (^2) . (filter even))


id' :: a -> a 
id' = \x -> x


compose :: [a -> a] -> (a -> a) 
compose = foldr (.) id'

double :: Int -> Int
double x = 2 * x

add_ten :: Int -> Int
add_ten x = x + 10


{-
Compose is read as a function takes in a list of functions a->a that returns a function a -> a

foldr then composes all the items in list using the id 

-}


------------------ Binary String Transmitter ------------------

-- First we create a type Bit
type Bit = Int


{-
A binary number, represented as a list of bits, 
can be converted into an integer by simply evaluating the appropriate weighted sum:
-}

bin2int :: [Bit] -> Int 
bin2int bits = sum [w*b | (w,b) <- zip weights bits] 
                     where weights = iterate (*2) 1

{-
higher-order library function iterate 
produces an infinite list by applying a function 
an increasing number of times to a value

bin2int [1,0,1,1] 
13

However bit2int can be written more simply
using algebra and [a,b,c,d]



(1 * a) +(2 * b) +(4 * c) +(8 * d) 
= { simplifying 1 * a } 
a +(2 * b) +(4 * c) +(8 * d)

= { factoring out 2 *} 
a + 2 * (b +(2 * c) +(4 * d))

= { factoring out 2 *} 
a + 2 * (b + 2 * (c +(2 * d)))

= { complicating d } 
a + 2 * (b + 2 * (c + 2 * (d + 2 * 0)))

This essentially says that the end function is very similar to using the cons function
which means when one has a list, one can use fold r

-}


bin2int' :: [Bit] -> Int 
bin2int' = foldr (\x y -> x + 2*y) 0

{-
one can go from Int to binary by using the remainder
13 to binary

13 divided by 2 = 6 remainder 1
6 divided by  2 = 3 remainder 0
3 divided by 2  = 1 remainder 1
1 divided by 2  = 0 remainder 1  

so
-}

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

{-
> int2bin 13 
[1,0,1,1]


We however need to make sure that each Bit has a max of 8 bits
sso
-}

make8 :: [Bit] -> [Bit] 
make8 bits = take 8 (bits ++ repeat 0)

{-
Understand that (bits ++ repeat 0) creates an infinite list using lazy evaluation
but ensures that only the first 8 are taken

> make8 [1,0,1,1] 
[1,0,1,1,0,0,0,0]
-}

------------------ Transmission ------------------

{-
Now we create a function to encode a list of characters as a list of bits

We will be using unicode
Converting each unicode number into an 8-bit binary number
then concatenating each numbers together to produce list of bits

-}
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)


{-

> encode "abc"
[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

remember ord changes char into unicode number
into2bin changes number to unicode
make 8 makes sure number is at max 8 bits long which is fine because
ord only goes up to 1 Byte which is 8 bits
the end result maps to a list
which is then concatenated at the end


-}


chop8 :: [Bit] -> [[Bit]]
chop8 [] = [] 
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String 
decode = map (chr . bin2int) . chop8

{-
> decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] 
"abc"

-}
channel :: [Bit] -> [Bit] 
channel = id


transmit :: String -> String 
transmit = decode . channel . encode

{-
cannot get to work with st

transmit' :: String -> String 
transmit' st = decode . (channel . (encode st))
-}


------------------ Voting algorithms ------------------

{-
First past the post

votes :: [String] 
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

Blue is winner as it has 3

-}
votes :: [String] 
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int 
count x = length . filter (== x)

{-
> count "Red" votes 
2

-}


rmdups :: Eq a => [a] -> [a]
rmdups []     = [] 
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

{-
> rmdups votes 
["Red", "Blue", "Green"]
-}



result :: Ord a => [a] -> [(Int,a)] 
result vs = sort [(count v vs, v) | v <- rmdups vs]


winner :: Ord a => [a] -> a 
winner = snd . last . result


{-
> result votes 
[(1,"Green"), (2,"Red"), (3,"Blue")]

> winner votes 
"Blue"

-}


{-
Alternative vote
-}


ballots :: [[String]] 
ballots = [["Red", "Green"],["Blue"], ["Green", "Red", "Blue"], 
            ["Blue", "Green", "Red"], ["Green"]]



rmempty :: Eq a => [[a]] -> [[a]] 
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]] 
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a] 
rank = map snd . result . map head

{-
> rank ballots 
["Red", "Blue", "Green"]
-}


winner' :: Ord a => [[a]] -> a 
winner' bs = case rank (rmempty bs) of
                  [c] -> c 
                  (c:cs) -> winner' (elim c bs)


{-
> winner’ ballots 
"Green"
-}







































