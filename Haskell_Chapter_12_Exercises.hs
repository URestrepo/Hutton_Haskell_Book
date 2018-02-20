-- 12.5 Exercises
{-


1. Define an instance of the Functor class for the following type of binary trees that have data in their nodes:

data Tree a = Leaf | Node (Tree a) a (Tree a) 
            deriving Show


data Tree a = Leaf a | Node (Tree a) (Tree a) 
            deriving Show

instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x) 
  fmap g (Node l r) = Node (fmap g l) (fmap g r)


leaf

Node (Leaf) 1 (Leaf)

Node (Node (Leaf) 1 (Leaf)) 2 (Node (Leaf) 3 (Leaf))
-}
data Tree a = Leaf | Node (Tree a) a (Tree a) 
            deriving Show

instance Functor Tree where
   fmap g (Leaf) = Leaf
   fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

{-
To Test Answer

> fmap (+10) (Node (Node (Leaf) 1 (Leaf)) 2 (Node (Leaf) 3 (Leaf)))
Node (Node Leaf 11 Leaf) 12 (Node Leaf 13 Leaf)
-}



{-
2. Complete the following instance declaration to make the partially-applied function type (a ->) into a functor:

instance Functor ((->) a) where ...

Hint: first write down the type of fmap, and then think if you already know a library function that has this type.

instance Functor [] where
-- fmap :: (a -> b) -> [a] -> [b] 
   fmap = map

instance Functor Maybe where
-- fmap :: (a -> b) -> Maybe a -> Maybe b 
   fmap _ Nothing = Nothing 
   fmap g (Just x) = Just (g x)

-}
-- ***** This is already defined in GHC.Base
-- instance Functor ((->) a) where 
-- -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
--    fmap = (.)

{-
3. Define an instance of the Applicative class for the type (a ->). If you
are familiar with combinatory logic, you might recognize pure and <*> for this type as being the well-known K and S combinators.

class Functor f => Applicative f where 
   pure :: a -> f a 
   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where 
   -- pure :: a -> Maybe a 
   pure = Just

   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
   Nothing <*> _ = Nothing 
   (Just g) <*> mx = fmap g mx

instance Applicative ((->) a) where 
   -- pure :: b -> (a -> b)
   pure = const

   -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
   g <*> h = \x -> g x (h x)

-}

{-
4. There may be more than one way to make a parameterized type into an applicative functor. For example, the library Control.Applicative provides an alternative ‘zippy’ instance for lists, in which the function pure makes an infinite list of copies of its argument, and the operator <*> applies each argument function to the corresponding argument value at the same position. Complete the following declarations that implement this idea:

newtype ZipList a = Z [a] deriving Show instance Functor ZipList where

-- fmap :: (a -> b) -> ZipList a -> ZipList b fmap g (Z xs) = ...

instance Applicative ZipList where -- pure :: a -> ZipList a pure x = ...

-- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b (Z gs) <$> (Z xs) = ...

The ZipList wrapper around the list type is required because each type can only have at most one instance declaration for a given class.


-}
newtype ZipList a = Z [a] deriving Show 

instance Functor ZipList where
   -- fmap :: (a -> b) -> ZipList a -> ZipList b 
   fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where 
   -- pure :: a -> ZipList a 
   pure x = Z (repeat x)

   -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b 
   (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

{-
5. Work out the types for the variables in the four applicative laws.


-}

{-
6. Define an instance of the Monad class for the type (a ->).


-}

{-
7. Given the following type of expressions

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

that contain variables of some type a, show how to make this type into instances of the Functor, Applicative, and Monad classes. With the aid of an example, explain what the >>= operator for this type does.


-}

{-
8. Rather than making a parameterised type into instances of the Functor, Applicative and Monad classes in this order, in practice it is sometimes simpler to define the functor and applicative instances in terms of the monad instance, relying on the fact that the order in which declarations are made is not important in Haskell. Complete the missing parts in the following declarations for the ST type using the do notation.

instance Functor ST where

-- fmap :: (a -> b) -> ST a -> ST b fmap g st = do ...

instance Applicative ST where -- pure :: a -> ST a pure x = S (\s -> (x,s)) -- (<*>) :: ST (a -> b) -> ST a -> ST b stf <*> stx = do ...

instance Monad ST where

-- (>>=) :: ST a -> (a -> ST b) -> ST b st >>= f = S (\s -> let (x,s’) = app st s in app (f x) s’)
-}

