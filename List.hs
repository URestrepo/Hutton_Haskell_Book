divide0 :: [Int] -> [Int]
divide0 [] = []
divide0 [0] = [0]
divide0 (x:xs) = x`div`0 : divide0 xs