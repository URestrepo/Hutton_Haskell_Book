{-
8.9 Exercises

1. In a similar manner to the function add, 
define a recursive multiplication function 
mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:

Hint: make use of add in your definition. 

(*&) :: Int -> Int -> Int
m *& 0 = 0
m *& n = m + (m *& (n-1))

-}
data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0 
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- To add, use nat2int then convert back int2nat
-- 

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

-- However can be written more efficiently using recursion
add' :: Nat -> Nat -> Nat
add' Zero n     = n
add' (Succ m) n = Succ (add' m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)


{-

2. Although not included in appendix B, the standard prelude defines

data Ordering = LT | EQ | GT

together with a function

compare :: Ord a => a -> a -> Ordering

that decides if one value in an ordered type is
 less than (LT),
 equal to (EQ),or
  greater than (GT) another value. 

  Using this function, redefine the function occurs :: Ord a => a -> Tree a -> Bool 
  for search trees. 
  Why is this new definition more efficient than the original version?

-}


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)            = x == y
occurs x (Node left y right) | compare x y == EQ = True
                             | compare x y == LT = occurs x left
                             | compare x y == GT = occurs x right



t :: Tree Int 
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 
            (Node (Leaf 6) 7 (Leaf 9))
{-
> occurs 1 t
True

> occurs 2 t 
False

The function occurs is faster because it cuts the search options by half
This is assuming that the tree is, I think, a binary search tree whereby 
the components are ordered such that if parent is equal or less than the number being compared to
the tree goes to the right creating a child on the right and if greater than parent, 
the number will become a child on the right
-}

{-
3. Consider the following type of binary trees:

data Tree a = Leaf a | Node (Tree a) (Tree a)

Let us say that such a tree is balanced if the number of leaves 
in the left and right subtree of every node differs by at most one, 
with leaves themselves being trivially balanced. 
Define a function 
balanced :: Tree a -> Bool 
that decides if a binary tree is balanced or not.

Hint: first define a function that returns the number of leaves in a tree.
-}

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

countLeaves :: Tree' a -> Int
countLeaves (Leaf' x) = 1
countLeaves (Node' l r) = countLeaves l + countLeaves r 


balanced :: Tree' a -> Bool
balanced (Leaf' x) = True
balanced (Node' l r) | abs (countLeaves l - countLeaves r) > 1 = False
                     | otherwise = (balanced l) && (balanced r)
{-

Node' (Node' (Leaf' 1) (Leaf' 2)) (Node' (Leaf' 3) (Leaf' 4))

Node' (Node' (Leaf' 5) (Leaf' 6)) (Node' (Leaf' 7) (Leaf' 8))


> balanced (Node' (Node' (Node' (Node' (Leaf' 5) (Leaf' 6)) (Leaf' 6)) (Leaf' 6)) (Node' (Leaf' 7) (Leaf' 8)))
False

t' :: Tree' Int
t' = Node' (Node' (Leaf' 3) (Leaf' 4)) 
           (Node' (Leaf' 2) (Node' (Leaf' 5) (Leaf' 1)))

-}


{-
4. Define a function 
balance :: [a] -> Tree a 
that converts a non-empty
list into a balanced tree. 
Hint: first define a function that splits a list into 
two halves whose length differs by at most one.

halve' :: [a] -> ([a], [a])
halve' xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

-}

halve :: [a] -> ([a], [a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)


balance :: [a] -> Tree' a 
balance [x] = Leaf' x
balance xs = Node' (balance l1) (balance l2)
              where (l1, l2) = halve xs


-- balance 
-- balance [x,y,z] = Node' (Node 
{-
5. Given the type declaration

data Expr = Val Int | Add Expr Expr

define a higher-order function

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

such that folde f g replaces each Val constructor 
in an expression by the function f, and each Add constructor by the function g.
-}


data Expr = Val Int | Add Expr Expr | Multiply Expr Expr deriving Show



value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y


type Cont = [Op]

data Op = EVAL Expr | ADD Int | MULTIPLY Expr deriving Show


eval' :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value' :: Expr -> Int
value' e = eval' e []

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val e) = f e
folde f g (Add m n) = g (folde f g m) (folde f g n)


{-
6. Using folde, 
define a function 

eval :: Expr -> Int 

that evaluates an expression to an integer value, and 
a function 
size :: Expr -> Int 
that calculates the number of values in an expression.

-}

eval'' :: Expr -> Int
eval'' = folde id (+)

size :: Expr -> Int
size e = folde (\x -> 1) (+) e

{-
7. Complete the following instance declarations:

instance Eq a => Eq (Maybe a) where
...
instance Eq a => Eq [a] where 
...

Since I am unsure as to if I can get this to work in terminal, I will leave my answer in the comments

instance Eq a => Eq (Maybe a) where




instance Eq a => Eq [a] where 



-}


{-
8. Extend the tautology checker to support the use of logical disjunction (∨)
and equivalence (⇔) in propositions.
-}

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

type Subst = Assoc Char Bool

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equivalence Prop Prop
          deriving Show

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Var 'B')

p6 :: Prop
p6 = Or (Var 'A') (Not (Var 'A'))

p7 :: Prop
p7 = And (Var 'A') (Var 'A')

p8 :: Prop
p8 = Equivalence (Var 'A') (Var 'A')

eval :: Subst -> Prop -> Bool
eval _ (Const b)         = b
eval s (Var x)           = find x s
eval s (Not p)           = not (eval s p)
eval s (And p q)         = eval s p && eval s q
eval s (Or p q)          = eval s p || eval s q
eval s (Imply p q)       = eval s p <= eval s q
eval s (Equivalence p q) = eval s p == eval s q


vars :: Prop -> [Char]
vars (Const _)          = []
vars (Var x)            = [x]
vars (Not p)            = vars p
vars (And p q)          = vars p ++ vars q
vars (Or p q)           = vars p ++ vars q
vars (Imply p q)        = vars p ++ vars q
vars (Equivalence p q)  = vars p ++ vars q


type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
          where
            range     = [0..(2^n)-1]
            make n bs = take n (bs ++ repeat 0)
            conv 0    = False
            conv 1    = True


bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False:) bss ++ map (True:) bss
               where bss = bools' (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = [] 
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
              where vs = rmdups (vars p)


isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

{-
To test

> isTaut p6
True

> isTaut p8
True
-}


{-
9. Extend the abstract machine to support the use of multiplication.

I added Multiply on the top data type and in Op

data Expr = Val Int | Add Expr Expr | Multiply Expr Expr deriving Show

data Op = EVAL Expr | ADD Int | MULTIPLY Expr deriving Show



-}

value' :: Expr -> Int
value' (Val n)   = n
value' (Add x y) = value x + value y

eval''' :: Expr -> Cont -> Int
eval'''' (Val n) c   = exec c n
eval''' (Add x y) c = eval''' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval''' y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)































