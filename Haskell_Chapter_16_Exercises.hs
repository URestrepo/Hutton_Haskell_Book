-- 16.9 Exercises
{-
1. Show that add n (Succ m) = Succ (add n m), by induction on n.
-}

{-
2. Using this property, together with add n Zero = n, show that addition is
commutative, add n m = add m n, by induction on n.
-}

{-
3.

Using the following definition for the library function that decides if all elements of a list satisfy a predicate

all p [] = True 
all p (x:xs) = p x && all p xs

complete the proof of the correctness of replicate by showing that it produces a list with identical elements, all (== x) (replicate n x),
by induction on Using the definition
Hint: show that the property is always True.
-}

{-
4. 
Using the definition

[] ++ ys     = ys 
(x:xs) ++ ys = x : (xs ++ ys)

verify the following two properties, by induction on 

xs: xs ++ [] = xs 
xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

Hint: the proofs are similar to those for the add function.
-}

{-
5. 
Using the above definition for ++, together with

take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

show that take n xs ++ drop n xs = xs, by simultaneous induction 
on the integer n --img-- 0 and the list xs. 

Hint: there are three cases, one for each pattern of arguments in the definitions of take and drop.
-}

{-
6.

Given the type declaration 

data Tree = Leaf Int | Node Tree Tree

show that the number of leaves in such a tree is always one greater than the number of nodes, 
by induction on trees. Hint: start by defining functions that count the number of leaves and nodes in a tree.

-}

{-
7. Verify the functor laws for the Maybe type. Hint: the proofs proceed by case analysis, and do not require the use of induction.
-}

{-
8. Given the type and instance declarations below, verify the functor laws for the Tree type, by induction on trees.

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where 
-- fmap :: (a -> b) -> Tree a -> Tree b
fmap g (Leaf x) = Leaf (g x) 
fmap g (Node l r) = Node (fmap g l) (fmap g r)

-}

{-
9. Verify the applicative laws for the Maybe type.
-}

{-
10. Verify the monad laws for the list type. Hint: the proofs can be completed using simple properties of list comprehensions.

-}

{-
11. Given the equation comp’ e c = comp e ++ c, show how to construct the

recursive definition for comp’, by induction on e.
-}

{-

-}





