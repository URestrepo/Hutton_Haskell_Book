-- Haskell
--Chapter 2

{-
Haskell Follows Order of Operations

2+3*4
- 14

(2+3)*4
20

sqrt (3^2 + 4^2)


Standard prelude (operations)

head
- find first element of list

tail
- everything after "head"

[list] !! 2
- find 2nd element of list, counting from 0

take n [list]
- take n elements from list 

drop n [list]
- remove first n elements of list

length [list]
- calculate length of list

sum [list]
- sum list

product [list]
- multiply list

[list] ++ [list]
- append 2 lists


reverse [list]

EX
head [1,2,3,4,5]
1


tail [1,2,3,4,5]
[2,3,4,5]


[1,2,3,4,5] !! 2
3

take 3 [1,2,3,4,5]

drop 3 [1,2,3,4,5]
[4,5]

length [1,2,3,4,5]
5

sum [1,2,3,4,5]
15

product [1,2,3,4,5]

[1,2] ++ [3,4,5]
[1,2,3,4,5]

reverse [1,2,3,4,5]
[5,4,3,2,1]


FUNCTION APPLICATION


-}

double x = x + x

quadruple x = double (double x)

{-
> quadruple 10
40

take (double 2) [1,2,3,4,5]


-}

factorial n = product [1 .. n]

average ns = sum ns `div` length ns
-- or 
-- average ns = div (sum ns) (length ns)

{-
`function`
put in `` the function between two arguments to do the same as function (arg) (arg)



> factorial 10
3628800 

> average [1,2,3,4,5]
3

Layout rule

each script def must begin  at same column level
All Defs must begin lowercase


-}


a = b + c
    where 
        b = 1 
        c = 2
















