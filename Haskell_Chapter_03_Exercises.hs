{-
3.11 Exercises
1. What are the types of the following values?

['a','b','c'] 
list of Chars

GHCi Answer
['a','b','c'] :: [Char]

('a','b','c') 
Tuple of arity 3

GHCi Answer
('a','b','c') :: (Char, Char, Char)

[(False,'O'),(True,'1')]
List of Tuple (Bool, Char) Arity 2

GHCi Answer
[(False,'O'),(True,'1')] :: [(Bool, Char)]

([False,True],['0','1']) 
Tuple of arity 2 ([Bool], [Char])

GHCi Answer
([False,True],['0','1']) :: ([Bool], [Char])

[tail, init, reverse]
list of functions?

GHCi Answer
[tail, init, reverse] :: [[a] -> [a]]



2. Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.

bools :: [Bool] 
bools = [True]

nums :: [[Int]] 
nums = [[1,2,3]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply f x = f x

3. What are the types of the following functions? Hint, create function definitions in form ::

second xs = head (tail xs)
:: [a] -> a


swap (x,y) = (y,x) 
:: (t, t1) -> (t1, t)


pair x y = (x,y) 
pair :: a -> b -> (a, b)

double x = x*2
double :: num a => a -> a

palindrome xs = reverse xs == xs 
:: Eq => [a] -> Bool

twice' f x = f (f x)
twice' :: ( a -> a) -> a

Hint: take care to include the necessary class constraints in the types if the functions are defined using overloaded operators.

4. Check your answers to the preceding three questions using GHCi.

5. Why is it not feasible in general for function types to be instances of the Eq class? When is it feasible? Hint: two functions of the same type are equal if they always return equal results for equal arguments.
-}


add' :: Int -> Int -> Int -> Int
add' a b c = a + b + c

swap' :: (t, t1) -> (t1, t)
swap' (x,y) = (y,x) 

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

pair' :: a -> b -> (a, b)
pair' x y = (x,y) 


double' :: Num a => a -> a
double' x = x*2


palindrome' :: Eq a => [a] -> Bool
palindrome' xs = reverse xs == xs 


second' :: [a] -> a
second' xs = head (tail xs)







