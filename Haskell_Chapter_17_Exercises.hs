-- 16.9 Exercises
{-

1. Suppose that we extend the language of arithmetic expressions with simple primitives for throwing and catching an exception, as follows:

data Expr = Val Int
    | Add Expr Expr 
    | Throw 
    | Catch Expr Expr

Informally, Catch x h behaves as the expression x unless evaluation of x throws an exception, in which case the catch behaves as the handler expression h. An exception is thrown if evaluation of Throw is attempted. To define a semantics for this extended language, we first recall the Maybe type:

data Maybe a = Nothing | Just a


That is, a value of type Maybe a is either Nothing, which we view here as

an exceptional value, or has the form Just x, which we view as a normal value. Using this type, our original evaluation function for expressions can be rewritten to take account of exceptions as follows:

Using the approach described in this chapter, calculate a compiler for this language. Hint: this is a challenging exercise!


-}

{-



-}

{-



-}

{-



-}

{-



-}





