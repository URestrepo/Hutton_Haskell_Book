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


























































