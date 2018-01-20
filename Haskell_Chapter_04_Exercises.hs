{-
4.8 Exercises

1. Using library functions, define a function halve :: [a] -> ([a],[a])

that splits an even-lengthed list into two halves. For example:
> halve [1,2,3,4,5,6] ([1,2,3],[4,5,6])

-}
halve' :: [a] -> ([a], [a])
halve' xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

{-

2. Define a function third :: [a] -> a that returns the third element in a list that contains at least this many elements using:
-}
-- a. head and tail;
third' :: [a] -> a
third' xs = head (tail (tail xs))

-- b. list indexing !!;
third'' :: [a] -> a
third'' xs = xs !! 2

-- c. pattern matching.
third''' :: [a] -> a
third''' (_:_:x:xs) = x


{-
3. Consider a function safetail :: [a] -> [a] that behaves in the same

way as tail except that it maps the empty list to itself rather than producing an error. 
Using tail and the function null :: [a] -> Bool that decides if a list is empty or not, 
define safetail using:

-}
-- a. a conditional expression;
safetail' :: [a] -> [a]
safetail' xs = if null xs == True then xs else tail xs

-- b. guarded equations;
safetail'' :: [a] -> [a]
safetail'' xs | null xs == True = xs
              | otherwise = tail xs


-- c. pattern matching.
safetail''' :: [a] -> [a]
safetail''' [] = []
safetail''' xs = tail xs


-- 4. In a similar way to && in section 4.4, 
--show how the disjunction operator || can be defined in four different ways 
-- using pattern matching.
-- Answer 1
(||*) :: Bool -> Bool -> Bool
False ||* False = False
True ||* True   = True
True ||* False  = True
False ||* True  = True


-- Answer 2
(||**) :: Bool -> Bool -> Bool
False ||** False = False
_ ||** _ = True

-- Answer 3
(||***) :: Bool -> Bool -> Bool
True ||*** _ = True
False ||*** b = b

-- Answer 4 Piecewise Function
(||****) :: Bool -> Bool -> Bool
b ||**** c | b == c = b
           | otherwise = True



{-
5. Without using any other library functions or operators, 
show how the meaning of the following pattern matching definition for logical conjunction 
&& can be formalised using conditional expressions:

True && True = True
_    && _    = False

Hint: use two nested conditional expressions.
-}

(&&*) :: Bool-> Bool -> Bool
a &&* b = if a == b then
             if a == True then True
             else False
          else False

{-

6. Do the same for the following alternative definition, 
and note the difference in the number of conditional expressions that are required:

True && b = b 
False && _ = False
-}

(&&**) :: Bool-> Bool -> Bool
a &&** b = if a == True then b
           else False


{- 
7. Show how the meaning of the following curried function definition 
can be formalized in terms of lambda expressions:

mult :: Int -> Int -> Int -> Int 
mult x y z = x*y*z
-}

mult' :: Int -> Int -> Int -> Int
mult' = \x -> \y -> \z -> x*y*z

-- Reads, mult' takes in a function with x that takes in a function with y that takes a function with z

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

In the exercises for chapter 7 we will consider a more general version of this function that accepts card numbers of any length.
-}

luhnDouble :: Int -> Int 
luhnDouble x | 2*x > 9   = 2*x - 9
             | otherwise = 2*x


luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0 = True
             | otherwise                                           = False


-- To make code more organized, one can use the where (which allows you to define values)

luhn' :: Int -> Int -> Int -> Int -> Bool
luhn' a b c d | total `mod` 10 == 0 = True
              | otherwise           = False
                 where total = luhnDouble a 
                             + b
                             + luhnDouble c 
                             + d






















