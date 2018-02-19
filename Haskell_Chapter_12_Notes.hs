-- Haskell
--Chapter 1
-- Monads and More

{-


-}

------------------ Functors ------------------

-- Chapter will be on idea of abstracting common programming pattern

inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns


{-
> inc [1..9]
[2,3,4,5,6,7,8,9,10]

> sqr [1..9]
[1,4,9,16,25,36,49,64,81]

Understand that both functions are defined in same manner

empty list being mapped to itself, non-empty list, some function applied to head and recursively processing tail.


Pattern can be abstracted into
-}

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- Which would allow the previous functions to be written as

inc' = map (+1)
sqr' = map (^2)

-- The results will be the same as the previous answers

-- Idea of mapping over each element of data can be abstracted further to wide range of parameterized types
-- Class of types that support such mappings are called "functors"

-- In Haskell, this is described in prelude


-- class Functors' f where
--   fmap :: (a -> b) -> f a -> f b
{-


Above can be described as, for a parameterized type f to be instance of class Functor, it must support function fmap
of the specified type.

fmap takes in a function of type a -> b and a structure f a whose elements have type a,
and applies the function to each element to give a structure of type f b
whose elements are of type b.

The fact that f must be a parameterized type, (a type that takes another type as a parameter is determined
automatically during type inference by virtue of the application f to the types a and b in the specified type for fmap in class declaration)


type of lists can be made into functor by defining fmap to be the function map
-}


{-
instance Functor []- where
 -- fmap :: (a -> b) -> [a] -> [b]
   fmap = map


Symbol [] in declaration notes the list without a type parameter and based upon the fact that type [a] cab be written in more primative
form as the application [] a of the list type [] to parameter type a.

type fmap is implicit as Haskell does not allow type information in instance declarations.



instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just (g x)


data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
-- fmap :: (a -> b) -> Maybe a -> Maybe b 
fmap _ Nothing' = Nothing' 
fmap g (Just' x) = Just' (g x)


-}
data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
-- fmap :: (a -> b) -> Maybe a -> Maybe b 
  fmap _ Nothing' = Nothing' 
  fmap g (Just' x) = Just' (g x)

{-
> fmap (+1) 
Nothing Nothing

> fmap (*2) (Just 3) 
Just 6

> fmap not (Just False) 
Just True


User-defined types can also be made into functors. 
Example of binary trees that have data in their leaves:
-}

data Tree a = Leaf a | Node (Tree a) (Tree a) 
            deriving Show

{-
The parameterized type tree can be made into Functor by defining a function fmap
-}
instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x) 
  fmap g (Node l r) = Node (fmap g l) (fmap g r)
{-
> fmap length (Leaf "abc") 
Leaf 3

> fmap even (Node (Leaf 1) (Leaf 2)) 
Node (Leaf False) (Leaf True)

Many functors in Haskell in a sense use f a as a data structure that contains elements of type a (sometimes called container type)
and fmap applies function to each such element.

Know that IO is not a container types in normal sense as its values represent input/output actions whose internal structure we 
do not have access to.

It however can be made into functor


********* Note this will not function because it exists in GHC.Base ****************
instance Functor IO where
   -- fmap :: (a -> b) IO a -> IO b
   fmap g mx = do {x <- mx; return (g x)}



> fmap show (return True) 
"True"

Note that fmap can be used for structure with elements that are factorial.
Allowing people to use the same name for functions rather than creating a new instance.

Note we can defin generic functions that can be used with any functor.

Example inc, can be used to generalize any functorial type
-}
inc'' :: Functor f => f Int -> f Int
inc'' = fmap (+1)


{-
> inc'' (Just 1) 
Just 2

> inc'' [1,2,3,4,5] 
[2,3,4,5,6]

> inc'' (Node (Leaf 1) (Leaf 2)) 
Node (Leaf 2) (Leaf 3)
-}


------------------ Functor Laws ------------------


{-

To create functors, there are two laws that must be satisfied

fmap id      = id
fmap (g . h) = fmap g . fmap h


1. fmap requires to preserve identity function.

2. second equation states fmap preserves function composition.

Meaning if one were to reverse a list for example,  it would not satify second law.




-}

{-
This does not work as it requires one to declare their own list type
Will fix in future

instance Functor [] where 
-- fmap :: (a -> b) -> f a -> f b
   fmap g [] = [] 
   fmap g (x:xs) = fmap g xs ++ [g x]

> fmap id [1,2]
[2,1]

> id [1,2] 
[1,2]

> fmap (not . even) [1,2] 
[False,True]

> (fmap not . fmap even) [1,2] 
[True,False]

-}
------------------ Applicatives ------------------

{-
Suppose one wished to allow functions with any number of of arguments to be mapped, rather than restricted to single argument


fmap0 :: a -> f a 
fmap1 :: (a -> b) -> f a -> f b 
fmap2 :: (a -> b -> c) -> f a -> f b -> f c 
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d .
.
.

pure :: a -> f a

(<*>) :: f (a -> b) -> f a -> f b


pure converts a value into a structure of type f a.
<*>


**********something something add later***********

use of pure and <*>

pure g <*> x1 <*> x2 <*> ... <*> xn

the above represents applicative style.

g is a curried function that takes n arguments of type a1 .. an and produces type b

-}
-- class Functor f => Applicative f where
--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b

{-
Class of functions that support pure and <*> are called applicative functors or applicatives


-}
-- instance Applicative Maybe where
--    -- pure :: a -> Maybe a
--    pure = Just
--    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--    Nothing <*> _   = Nothing
--    (Just g) <*> mx = fmap g mx

{-
> pure (+1) <*> Just 1
Just 2

> pure (+) <*> Just 1 <*> Just 2
Just 3

> pure (+) <*> Nothing <*> Just 2
Nothing

instance  Applicative [] where
   -- pure :: a -> [a]
   pure x = [x]

   -- (<*>) :: [a -> b] -> [a] -> [b]
   gs <*> xs = [g x | g <- gs, x <- xs]

> pure (+1) <*> [1,2,3] 
[2,3,4]

> pure (+) <*> [1] <*> [2] 
[3]

> pure (*) <*> [1,2] <*> [3,4] 
[3,4,6,8]

-}
prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x*y | x <- xs, y <- ys] 

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys


{-
instance Applicative IO where 
   -- pure :: a -> IO a 
   pure = return

   -- (<*>) :: IO (a -> b) -> IO a -> IO b 
   mg <*> mx = do {g <- mg; x <- mx; return (g x)}

-}
getChars :: Int -> IO String 
getChars 0 = return [] 
getChars n = pure (:) <*> getChar <*> getChars (n-1)

{-
from prelude

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure [] 
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

Applicative Functors 4 laws

pure id <*> x = x
pure (g x)    = pure g <*> pure x
x <*> pure y  = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z

-}

------------------ Monads ------------------

data Expr = Val Int | Div Expr Expr

eval' :: Expr -> Int
eval' (Val n)   = n
eval' (Div x y) = eval' x `div` eval' y

{-
> eval' (Div (Val 1) (Val 0)) 
*** Exception: divide by zero

-}

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)


eval'' :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = case eval'' x of
          Nothing -> Nothing
          Just n -> case eval'' y of
                Nothing -> Nothing
                Just m  -> safediv n m

{-
> eval'' (Div (Val 1) (Val 0)) 
Nothing
-}




{-
Note this will not work because 
safediv is impure
safediv can be nothing or Just a

eval :: Expr -> Maybe Int
eval (Val n)   = pure n
eval (Div x y) = pure safediv <*> eval x <*> eval y 

to fix we use

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b 
mx >>= f = case mx of
            Nothing -> Nothing 
            Just x -> f x

>>= is called "bind"

-}

eval ::Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = eval x >>= \n ->
                 eval y >>= \m ->
                 safediv n m 

{-

> eval (Div (Val 1) (Val 0)) 
Nothing

> eval (Div (Val 6) (Val 2))
Just 3

>>= operator ensures that such an expression only succeeds if every component mi in the sequence succeeds. 
Moreover, the user does not have to worry about dealing with the possibility of failure at any point in the sequence, 
as this is handled automatically by the definition of the >>= operator.

-}
eval''' :: Expr -> Maybe Int 
eval''' (Val n)   = Just n 
eval''' (Div x y) = do n <- eval''' x 
                       m <- eval''' y 
                       safediv n m
{-
class Applicative m => Monad m where 
   return :: a -> m a 
   (>>=) :: m a -> (a -> m b) -> m b

   return = pure

monad is an applicative type

instance Monad Maybe where
   -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b 
   Nothing >>= _ = Nothing 
   (Just x) >>= f = f x

instance Monad [] where
   -- (>>=) :: [a] -> (a -> [b]) -> [b] 
   xs >>= f = [y | x <- xs, y <- f x]

-}
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x,y)

pairs' :: [a] -> [b] -> [(a,b)]
pairs' xs ys = xs >>= \x ->
               ys >>= \y ->
               return (x,y)
{-
> pairs [1,2] [3,4]
[(1,3),(1,4),(2,3),(2,4)]

-}
pairs'' :: [a] -> [b] -> [(a,b)] 
pairs'' xs ys = [(x,y) | x <- xs, y <- ys]
{-

-}

{-

-}

{-

-}

{-

-}

{-

-}

{-

-}


























































