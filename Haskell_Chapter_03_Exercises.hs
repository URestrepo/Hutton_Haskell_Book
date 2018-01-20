{-
3.11 Exercises
1. What are the types of the following values?


['a','b','c'] 

['a','b','c'] :: [Char] -- list of Chars

('a','b','c')

('a','b','c') :: (Char, Char, Char) -- Tuple of arity 3 

[(False,'O'),(True,'1')]

[(False,'O'),(True,'1')] :: [(Bool, Char)] -- List of Tuple arity 2

([False,True],['0','1']) 

([False,True],['0','1']) :: ([Bool], [Char]) -- Tuple of arity 2

[tail, init, reverse]

[tail, init, reverse] :: [[a] -> [a]] -- list of functions

-}


{- 2. Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.-} 

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

{- 3. What are the types of the following functions? Hint, create function definitions in form ::
Hint: take care to include the necessary class constraints in the types if the functions are defined using overloaded operators. -}

-- The question is actually the below the type definition. Question was reversed so that module can be loaded in Haskell

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


{-

4. Check your answers to the preceding three questions using GHCi.

5. Why is it not feasible in general for function types to be instances of the Eq class? 
When is it feasible? 
Hint: two functions of the same type are equal if they always return equal results for equal arguments.

To ask if two functions are the same is to ask the computer for a math proof that the functions are equivalent. 
If it cannot prove, it would also be difficult for computer to check every possible variation of inputs 
and check if they have the same output.


-}









