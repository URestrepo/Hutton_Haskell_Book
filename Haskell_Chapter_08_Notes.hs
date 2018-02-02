
-- Haskell
--Chapter 8
-- Declaring types and classes

{-
------------------ Type Declarations -----------------

Easiest way to declare new type = new name for existing type

-}

type String' = [Char]

-- Types must start with capital letter
-- Types can be nested as in one type can be declared in terms of another
{-
Example
Define a number of functions that transform coordinate positions, 
declare a position as a pair of integers, and 
a transformation as a function on positions:

-}

type Pos = (Int,Int)

type Trans = Pos -> Pos

{-
Types cannot be recursive
Example

type Tree = (Int, [Tree])
Though it is possible in different data type if recursive types are necessary
This will be introduced later


Types can be parameterized by other types
example
define functions that manipulate pairs of values of same type
-}

type Pair a = (a,a)

{-
One can create a sort of lookup table 
that associate keys of one type to values of another type

-}

type Assoc k v = [(k,v)]

{-
To be able to use the type though
one can create a function that returns the first value that is associated with the 
given key in the  table


-}

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

{-
example
> find 1 [(1,"love"),(2,"amour"),(3,"aqua")]
"love"

> find 2 [(1,"love"),(2,"amour"),(3,"aqua")]
"amour"

-}

------------------ Data Declarations ------------------
{-
Completely new types, as opposed to a synonym for an existing type, 
can be declared by specifying values using the "data" mechanism of Haskell

-}
data Bool' = False' | True'

{-
  | means or
and new values of the type are called constructors.
As with new types, names of new constructors must be capitalized

Same constructor name cannot be used in more than one type.

Names have no inherent meaning
Ex
Bool could be defined as
data A = B | C
As long as not used before, names are given meaning by programmers

Values of new types in Haskell can be used in precisely the same way 
as those of built-in types. Values of new types can freely 
be passed as arguments to functions, 
returned as results from functions, 
stored in data structures, and 
used in patterns.

-}
data Move = North | South | East | West deriving Show

-- Now for functions
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y)  = (x+1,y)
move West (x,y)  = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)


rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

{-
****** Note
If using GHCi, one must use phrase deriving Show at end of declaration
to ensure system can display values of new type;


Constructors in data declaration can have arguments
Example, type of shapes that comprise circles with given radius and rectangles
with dimensions

-}

data Shape = Circle Float | Rect Float Float

{-
Meaning
type Shape has values in form of
   Circle r
   Rect x y,

where r is floating point
where x and y are floating point

The types can be used to define functions on shapes such as to produce 
square of given size  and calculate area of shape

-}
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y


{-
Interestingly enough. Constructors Circle and Rect are actually
constructor functions, which produce type of shape from arguments
type Float

> :type Circle 
Circle :: Float -> Shape

> :type Rect 
Rect :: Float -> Float -> Shape

***** Functions vs Constructor Functions *****
- Constructor functions have no defining equations and 
exist purely for purposes of building pieces of data

Example
> negate 1.0
-1.0

Whereas
> negate Circle 1.0
1.0

Since 1.0 is already fully evaluated and cannot be simplified 
because there is no defining equations for Circle

Rather, 
> Circle 1.0 
is just a piece of data the same way 1.0 is just data

data declarations can be parametrized


-}
-- Already exists in prelude
-- data Maybe a = Nothing | Just a 


{-

Value of type maybe is either "Nothing" or 
"Just x" for some value of type "a"

Values of type maybe can be viewed as
type a may fail or succeed
"Nothing" represents failure
"Just"  represents success

Using type we can define safe versions of library functions of
   div
   head
One returns "Nothing" for invalid arguments
rather than producing an error

-}

safediv :: Int -> Int -> Maybe Int 
safediv _ 0 = Nothing 
safediv m n = Just (m `div` n) 

safehead :: [a] -> Maybe a
safehead [] = Nothing



safehead xs = Just (head xs)


------------------ 8.3 Newtype Declarations ------------------
{-
If new type has single constructor. It can use newtype

-}
-- New type of natural numbers, though up to programmer to enforce non-negative Int
-- newtype Nat = N Int


-- Though how is this different from below declarations

-- type Nat = Int

-- data Nat = n Int

{-
"newtype" means that Nat and Int are different types rather than synonyms

Haskell then ensures they cannot be mixed, for example  
using an Int when a natural number is expected


"newtype" brings in an efficiency benefit not available to "data"
Once compiler type checks, it is removed by compiler creating safety
without affecting performance


-}

------------------ Recursive Types ------------------
{-
Data types that are declared using data and "newtype" can also be recursive


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


{-
add' (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))

Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

idea is to use constructors from number until exhausted

add (Succ (Succ Zero)) (Succ Zero)
= { applying add }
Succ (add (Succ Zero) (Succ Zero))
= { applying add }
Succ (Succ (add Zero (Succ Zero)))
= { applying add }
Succ (Succ (Succ Zero))






-}


data List' a = Nil | Cons a (List' a)


len :: List' a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs 



data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int 
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 
            (Node (Leaf 6) 7 (Leaf 9))



occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r


flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


{-
occurs 3 t
= True

occurs 9 t
= True

occurs 10 t
= False


Trees may be made differently depending on situation

data Tree a = Leaf a | Node (Tree a) (Tree a) 

data Tree a = Leaf | Node (Tree a) a (Tree a) 

data Tree a b = Leaf a | Node (Tree a b) b (Tree a b) 

data Tree a = Node a [Tree a]

-}



------------------ Class and Instance Declarations ------------------
{-
Class is a collection of types that support certain overloaded operations called methods

Eq is a type of equality class

-}


class Eq' a where
      (==*), (/=*) :: a -> a -> Bool

      x /=* y = not (x ==* y)


{-
Declaration above states that something to be instance of class Eq', it must support equality
and inequality operators of the specified types.

Because default definition already has "/=" operator, declaring an instance only requires
a definition for "=="

Bool can be made into instance of Eq by

-}

instance Eq' Bool where
   False ==* False = True
   True  ==* True  = True
   _     ==* _     = False

{-
Only types that are declared using data and newtype mechanisms can be made 
into instances of classes. 

Classes can be extended to form new classes

For example Ord can be made as extention from Eq
-}

class Eq' a => Ord' a where
      (<#), (<=#), (>#), (>=#) :: a -> a -> Bool
      min, max             :: a -> a -> a


      min x y | x <=# y    = x
              | otherwise = y

      max x y | x <=# y    = y
              | otherwise = x


instance Ord' Bool where
   False <# True = True
   _     <# _    = False

   b <=# c = (b <# c) || (b == c)
   b ># c  = c <# b
   b >=# c = c <=# b


------------------ Derived Instances ------------------

{-
It is useful when types are declared to be part of instances of other built-in classes
This can be done using

deriving


ex
-}
-- Cannot get to work
-- *******************
-- data Bool'' = False' | True'
                  -- deriving (Eq, Ord, Show, Read)


{-
Regardless of not being able to get to work, the results should look like

> False == False 
True

> False < True 
True

> show False 
"False"

> read "False" :: Bool 
False

Note: use of "::" in the last example is required 
to resolve the type of the result, 
which in this case cannot be inferred from the context 
in which the function is used


Note reason why > False < True  == True is due 
-}

------------------ 8.6 Tautology checker ------------------
{-


-}
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop deriving Show

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


type Subst = Assoc Char Bool


eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q


vars :: Prop -> [Char]
vars (Const _)    = []
vars (Var x)      = [x]
vars (Not p)      = vars p
vars (And p q)    = vars p ++ vars q
vars (Imply p q)  = vars p ++ vars q

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


------------------ 8.7 Abstract machine ------------------

data Expr = Val Int | Add Expr Expr deriving Show

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y


type Cont = [Op]

data Op = EVAL Expr | ADD Int deriving Show


eval' :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c)  m = exec c (n+m)

value' :: Expr -> Int
value' e = eval' e []


{-
value' (Add (Add (Val 2) (Val 3)) (Val 4))
= eval' (Add (Add (Val 2) (Val 3)) (Val 4)) []
= eval' (Add (Val 2) (Val 3)) [EVAL (Val 4)]
= eval' (Val 2) [EVAL (Val 3), EVAL (Val 4)]
= exec [EVAL (Val 3), EVAL (Val 4)] 2
= eval' (Val 3) [(ADD 2), EVAL (Val 4)]
= exec [(ADD 2), EVAL (Val 4)] 3
= exec [EVAL (Val 4)] 2+3
= exec [EVAL (Val 4)] 5
= eval' (Val 4) [ADD 5]
= exec [ADD 5] 4
= exec [] 5+4
= exec [] 9
= 9
-}


























