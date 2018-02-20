import Data.List (sortBy)
import Data.Ord (comparing)

{-
9.11 Exercises

1. Redefine the combinatorial function choices using a list comprehension rather than using composition, concat, and map.

choices :: [a] -> [[a]]
choices = concat . map perms . subs
-}

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]


{-
2. Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool

that decides if one list is chosen from another, 
without using the combinatorial functions perms and subs. 
Hint: start by defining a function that removes the first occurrence of a value from a list.


isChoice [1,2,3] [1,2,3,4] == True
isChoice [1,2,3] [3,2,1] == True
isChoice [1,2,3] [3,2,4] == False
-}
remove_item :: Eq a => a -> [a] -> [a]
remove_item x []                 = []
remove_item x (y:ys) | x == y    = ys
                     | otherwise = y : remove_item x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] ys     = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (remove_item x ys)

double :: Int -> Int
double x = 2 * x

add_three :: Int -> Int
add_three x = x + 3

{-
3. What effect would generalizing the function split 
to also return pairs containing the empty list have on the behavior of solutions?


-}

data Op = Add | Sub | Mul | Div | Exp
-- data Op = Add | Sub | Mul | Div


instance Show Op where 
  show Add = "+" 
  show Sub = "-" 
  show Mul = "*" 
  show Div = "/"
  show Exp = "^"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                        brak (Val n) = show n
                        brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r


eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- choices :: [a] -> [[a]]
-- choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
  elem (values e) (choices ns) && eval e == [n]

e :: Expr
e = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]


exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                  l       <- exprs ls,
                  r       <- exprs rs,
                  e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- ops :: [Op]
-- ops = [Add,Sub,Mul,Div]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
      [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && x `mod` y == 0 
valid Exp x y = x /= 1 && y > 1

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = True
-- valid Mul _ _ = True
-- valid Div x y = y /= 0 && x `mod` y == 0

{-
4. Using the functions choices, exprs, and eval, verify that there are
33,665,406 possible expressions over the numbers 
1, 3, 7, 10, 25, 50, 
and that only 4,672,540 of these expressions evaluate successfully.

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                  l       <- exprs ls,
                  r       <- exprs rs,
                  e       <- combine l r]

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

> sum [1 | _ <- exprs [1]]
1

sum [1 | _ <- exprs [1,2]]
4
ns' <- choices ns, e <- exprs ns'
> sum [1 | xs <- exprs [1,3,7,10,25,50], _ <- choices xs]

 ns' <- choices ns, e <- exprs ns'

> length [x | xs <- choices [1,3,7,10,25,50], x <- exprs xs]

> sum [1 | xs <- choices [1,3,7,10,25,50], x <- exprs xs]
Stack overflow

> [1 | xs <- choices [1,3,7,10,25,50], x <- exprs xs,]

-- The number below also includes exponents
> sum [length xs | xs <-  map exprs (choices [1,3,7,10,25,50])]
101031156


-- The number below does not include exponents
> sum [length xs | xs <-  map exprs (choices [1,3,7,10,25,50])]
33665406

[ eval x | x <- [3,1,1+3,1-3,1*3,1/3,3+1,3-1,3*1,3/1]]

sum [x | x <- exprs [1,2]]
[x | xs <- choices [1,3], x <- exprs xs]


length (filter (\xs -> xs /= []) (map eval [x | xs <- choices [1,3,7,10,25,50], x <- exprs xs]))


> length (filter (\zs -> zs /= []) (map eval [ys | xs <- choices [1,3,7,10,25,50], ys <- exprs xs]))
245644


What does choices, exprs, and eval do?

eval
eval :: Expr -> [Int]
returns the overall value of an expression, empty if goes negative [], otherwise with positive value
> eval (App Add (Val 2) (Val 3)) 
[5]

> eval (App Sub (Val 2) (Val 3)) 
[]


Choices
choices :: [a] -> [[a]]
function that returns all choices from a list, 
which are given by all possible ways of selecting zero or more elements in any order
> choices [1,2]
[[],[2],[1],[1,2],[2,1]]


exprs
exprs :: [Int] -> [Expr]
returns all possible expressions whose list of values is precisely a given list
> exprs [1,2]
[1+2,1-2,1*2,1/2]

let t = possible_expressions [1,3,7,10,25,50]
-}

all_expressions :: [Int] -> [Expr]
all_expressions = concat . map exprs . choices


count_length :: [a] -> Int
count_length = length

-- My function
-- possible_expressions :: [Int] -> [[Int]]
-- possible_expressions = filter (\xs -> xs /= []) . map eval . all_expressions

-- 
possible_expressions :: [Int] -> [[Int]]
possible_expressions = filter (not . null) . map eval . all_expressions


possible_expressions' :: [Int] -> [Int]
possible_expressions' = concat . map eval . all_expressions

{-
> let t = possible_expressions [1,3,7,10,25,50]
> count_length t
10839369

-}


{-
5. Similarly, verify that the number of expressions that evaluate successfully increases to 
10,839,369 if the numeric domain is generalized to arbitrary integers. Hint: modify the definition of valid.

Use this function for valid

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = True
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0

> let t = possible_expressions [1,3,7,10,25,50]
> count_length t
4672540
-}


{-
6. Modify the final program to:

a. allow the use of exponentiation in expressions;

exprs [1,2]
[1+2,1-2,1*2,1/2,1^2]

-}


{-
b. produce the nearest solutions if no exact solution is possible;

-}

solutions''' :: [Int] -> Int -> Int -> [Expr]
solutions''' ns n x = [e | ns' <- choices ns, (e,m) <- results ns', (m <= (n + x)) && (m >= (n - x))]


expand_sol :: [Int] -> Int -> Int -> [Expr]
expand_sol ns n x | (not . null) sols = sols
                  | otherwise         = expand_sol ns n (x + 1)
                   where sols = solutions''' ns n x


{-
But can we make functions better

Use the new type from Hutton to create a tuple that allows one to have 
the expression and an Int of the result of a expression in a tuple
-}

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                     lx     <- results ls,
                     ry     <- results rs,
                     res    <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns  n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

all_sol_diff :: [Int] -> Int -> [(Expr,Int)]
all_sol_diff ns n = [(e, abs(m-n)) | ns' <- choices ns, (e,m) <- results ns']

{-
> all_sol_diff [1,2] 2
[(2,0),(1,1),(1+2,1),(2-1,1),(2/1,0)]


The above function will output a tuple of the expression and the difference between the answer the
expression provides and what we are looking from in the universe of all valid expressions.

Next we will create a function that will sort the list based on the difference.
We can extract the ones that have say the smallest difference

*** We will have to import "comparing" and "sortBy"
import Data.List (sortBy)
import Data.Ord (comparing)

This will allow use
sortBy (comparing snd) [(20, 20), (30,15)]
= [(30,15),(20,20)] 

Basically using function Sortby is a function that allows one to use comparing
which in turn allows one to compare using use defined types, in this case
a tuple with the list and value

*** also recall that snd is a function that returns second value of a tuple
-}

low_diff_sol_sort :: [(Expr,Int)] -> [(Expr,Int)]
low_diff_sol_sort = take 10 . sortBy (comparing snd)
                    

calculate_lowest_diff :: [(Expr,Int)] -> Int
calculate_lowest_diff xs = minimum [m | (e,m) <- xs]

output_low_diff :: [(Expr,Int)] -> [(Expr,Int)]
output_low_diff xs = filter (\(_,a) -> a == x) xs
                      where x = calculate_lowest_diff xs

{-
low_diff_sol_sort $ all_sol_diff [1,2,50] 90
= [(2*(50-1),8),((2*50)-1,9),(2*50,10),(1+(2*50),11),(2*(1+50),12)]

> calculate_lowest_diff (all_sol_diff [1,2] 2)
0

> output_low_diff (all_sol_diff [1,2] 2)
[(2,0),(2^1,0)]

-}

{-
c. order the solutions using a suitable measure of simplicity.

For this I will implement a solution that creates a new function that calculates relative difficulty of a type of calculation.
This function will be based ans is similar to the apply function
and then create function that orders the a list by relative complexity
-}

exprCmplxty :: Expr -> Int
exprCmplxty (Val n) = 0
exprCmplxty (App Add l r) = 1 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Sub l r) = 1 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Mul l r) = 10 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Div l r) = 10 + exprCmplxty l + exprCmplxty r
exprCmplxty (App Exp l r) = 100 + exprCmplxty l + exprCmplxty r

orderSolnsByCmplxty :: [Int] -> Int -> [Expr]
orderSolnsByCmplxty ns n = (map fst) $
                           sortBy (comparing snd) 
                           $ map (\(x,y) -> (x, exprCmplxty x)) (low_diff_sol_sort (all_sol_diff ns n))

orderSolnsByCmplxty' :: [Int] -> Int -> [Expr]
orderSolnsByCmplxty' ns n = (map fst) $
                           sortBy (comparing snd) 
                           $ map (\(x,y) -> (x, exprCmplxty x)) (output_low_diff (all_sol_diff ns n))

{-
> orderSolnsByCmplxty' [1,2,50] 90
[2*(50-1)]

> orderSolnsByCmplxty' [1,2,50] 50
[50,50/1,50/(2-1)]

-}

{-

-}

{-
Scratch Notes and Questions

Things to ask/tell C
- 1 tell that I could not get the right answer because valid function included the optimizations that
Hutton had, which excluded the ones that were counted

- 2 ask why there would be a division by 0 error in valid 

valid Div x y = y /= 0 && x `mod` y == 0
vs
valid Div x y = x `mod` y == 0 && y /= 0

- 3. ask if the order of a comprehension matters

[e | ns' <- choices ns, (e,m) <- results ns', m == n]
vs
[e | (e,m) <- results ns',   nns' <- choices ns, m == n]

- 4 Why does sum have stack overflow problem while length does not?

> length [x | xs <- choices [1,3,7,10,25,50], x <- exprs xs]
33665406
> sum [1 | xs <- choices [1,3,7,10,25,50], _ <- exprs xs]
Stack overflow
-}

































