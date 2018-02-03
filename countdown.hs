
-- Haskell
--Chapter 9
-- The Countdown Problem

{-
The problem:

Given a sequence of numbers and a target number, 
attempt to construct an expression whose value is the target, 
by combining one or more numbers from the sequence using 
addition, subtraction, multiplication, division and parentheses.


-}


------------------ 9.2 Arithmetic Operators ------------------

-- Declare a type for the 4 arithmetic operators
-- Make type showable
data Op = Add | Sub | Mul | Div


instance Show Op where 
  show Add = "+" 
  show Sub = "-" 
  show Mul = "*" 
  show Div = "/"


-- Define function "valid" that decides if application of operator
-- to two positive naturals gives another positive natural
-- and  define function "apply" that actually performs functions

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0 

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y



------------------ 9.3 Numeric Expressions ------------------

-- Declare type for numeric expressions 
-- Can be Integer or application of operator

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                        brak (Val n) = show n
                        brak e       = "(" ++ show e ++ ")"

{-
This should produce
> show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
"1+(2*3)"

-}

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r


eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]


{-
> eval (App Add (Val 2) (Val 3)) 
[5]

> eval (App Sub (Val 2) (Val 3)) 
[]

-}
-- define function that returns all possible combinations of excluding or including
-- each element of list

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

{-
> subs [1,2,3] 
[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

> interleave 1 [2,3,4] 
[[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]

> perms [1,2,3] 
[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-}

choices :: [a] -> [[a]]
choices = concat . map perms . subs

{-
> choices [1,2,3] 
[[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1], 
[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}


------------------ Formalizing the Problem ------------------

-- Create function "solution" that formalizes
-- what it means to solve an instance of countdown problem

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
  elem (values e) (choices ns) && eval e == [n]

e :: Expr
e = (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))


{-
> solution e [1,3,7,10,25,50] 765 
True

-}



------------------ Brute Force Solution ------------------

-- Define function that returns all possible ways of splitting a list
-- into 2 non-empty lists that append

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]


{-
> split [1,2,3,4] 
[([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

-}

-- define function that returns all possible expressions whose list is 
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                  l       <- exprs ls,
                  r       <- exprs rs,
                  e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]


{-
> combine (Val 1) (Val 2)
[1+2,1-2,1*2,1/2]

> combine (App Add (Val 1) (Val 2)) (Val 3)
[(1+2)+3,(1+2)-3,(1+2)*3,(1+2)/3
-}


solutions :: [Int] -> Int -> [Expr]
solutions ns n =
      [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

------------------ Combining Generation and Evaluation ------------------

-- "solutions" generates many combinations that will fail "valid" function
-- Can one combine generation and evaluation functions


type Result = (Expr,Int)

-- define function "results" that returns all possible results
-- that pass valid

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

-- main = print $ solutions' [1,3,7,10,25,50] 952

main :: IO ()
main = print $ solutions' [1,3,7,10,25,50] 765





















