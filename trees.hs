-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b) deriving Show

data Tree a = Node a [Tree a] deriving Show




-- data Shape = Circle Float | Rect Float Float -- deriving Ord
data Shape = Circle Float | Rect Float Float deriving Ord


instance Ord Shape where
   area (Rect x y) < area (Rect x y) = True
   _ >= _ = False

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

