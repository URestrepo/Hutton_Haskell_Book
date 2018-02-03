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

data Op = Add | Sub | Mul | Div


instance Show Op where 
  show Add = "+" 
  show Sub = "-" 
  show Mul = "*" 
  show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

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

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
      [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

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

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0 



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


[ eval x | x <- [3,1,1+3,1-3,1*3,1/3,3+1,3-1,3*1,3/1]]

-}
































































