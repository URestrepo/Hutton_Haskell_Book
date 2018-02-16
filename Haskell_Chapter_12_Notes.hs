-- Haskell
--Chapter 1
-- Monads and More

{-


-}

------------------ Functors ------------------

-- Chapter will be on idea of abstracting common programming pattern

inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns


{-
Understand that both functions are defined in same manner

empty list being mapped to itself, non-empty list, some function applied to head and recursively processing tail.


Pattern can be abstracted into
-}

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- Which would allow the previous functions to be written as

inc' = map (+1)
sqr' = map (^2)















































