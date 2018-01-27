
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
data Bool' = False | True

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
data Move = North | South | East | West

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


data Nat = Zero | Succ Nat





















































