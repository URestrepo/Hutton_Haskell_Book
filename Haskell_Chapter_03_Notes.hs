-- Haskell
--Chapter 3
--Types and Classes

{-

type
- collection of related values

Ex
Bool

v :: T 
- means  v is a value in the type T
- it reads v has type T.

False :: Bool
-False has type Bool


Also
notation e :: T 
- means that evaluation of the expression e will produce a value of type T.


Sum up:
f :: A -> B     e:: A
________________________
        f e :: B

f is a function that maps argument of type A to results of type B.
e is an expression of Type A, then f e has a type B



- typing rule for a conditional expression 
requires that both possible results have the same type



__________

> :t not
 not :: Bool -> Bool

> :type False 
False :: Bool

> :type not False 
not False :: Bool


------------------GENERAL Types------------------

Bool
- False
- True

Char
''

String
""

Int (fixed-precision integers)
values of type Int in the range –2 63 to 2 63 – 1

Integer (arbitrary-precision integers)
type contains all integers, with as much memory as necessary being used for their storage


Float
- single-precision floating-point numbers 
with a fixed amount of memory being used for their storage

Double 
– double-precision floating-point numbers with twice as much memory as float 
and is used for storage of these numbers to increase their precision



------------------List Types------------------

list
- sequence of elements of same type

[T]
list with type T

Examples:
[False,True,False] :: [Bool] 
['a','b','c','d'] :: [Char] 
["One","Two","Three"] :: [String]


* list [] of length zero is called the empty list

*[[]] and [] are different lists, 
the former being a singleton list comprising the empty list as its only element, 
and the latter being simply the empty list that has no elements



------------------TUPLE TYPES------------------
tuple 
is a finite sequence of components of possibly different types

components being enclosed in round parentheses and separated by commas

number of components in a tuple is called its arity

tuple () of arity zero is called the empty tuple

Tuples of arity one, such as (False), 
are not permitted because they would conflict 
with the use of parentheses to make the evaluation order explicit


------------------FUNCTION TYPES------------------
Function
mapping from arguments of one type to results of another type

Example
T1 -> T2

even :: Int -> Bool


-}

--note function does not work
add' :: (Int,Int) -> Int
add' (x,y) = x+y

--note function does not work
zeroto' :: Int -> [Int]
zeroto' n = [0..n]

{-

Haskell convention of preceding function 
definitions by their types, serves as useful documentation.


-}


{-
------------------CURRIED FUNCTIONS------------------

Functions can have multiple arguments by way of 
currying
allowing functions to return functions as results



-}
add'' :: Int -> (Int -> Int) 
add'' x y = x+y

{-

In above example 
type states that add’ is a function that takes an argument of type Int, 
and returns a result that is a function of type Int -> Int

The definition itself states that add’ takes an integer x 
followed by an integer y, and returns the result x+y.

add’ takes an integer x and returns a function,
 which in turn takes an integer y and returns the result x+y.

-}

mult' :: Int -> (Int -> (Int -> Int))
mult' x y z = x*y*z


























