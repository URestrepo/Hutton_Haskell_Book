import Data.Char 
import Data.List 
import System.IO
-- import System.Random (randomRIO) 
import System.Random hiding (next)

------------------ 11.3 Exercises ------------------


size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)


full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
              os = length (filter (==O) ps)
              xs = length (filter (==X) ps)
              ps = concat g  

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]



wins_list :: Player -> Grid -> [[Player]]
wins_list p g = (rows ++ cols ++ dias)
           where
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]


won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = 
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4)-1) '-']


showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside = foldr1 (zipWith (++))
                bar    = replicate 3 "|"



showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   ","   ","   "]
showPlayer X = ["   "," X ","   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys


valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B


move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)


getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

cls :: IO ()
cls = putStr "\ESC[2J"

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g   = putStrLn "Its a draw. \n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    [] -> do putStrLn "ERROR: Invalid Move"
                             run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "


data Tree a = Node a [Tree a]
              deriving Show

-- gametree :: Grid -> Player -> Tree Grid
-- gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
   | won g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2) -1)]]


prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]



depth :: Int 
depth = 9

-- minimax :: Tree Grid -> Tree (Grid,Player)
-- minimax (Node g [])
--    | wins O g  = Node (g,O) []
--    | wins X g  = Node (g,X) []
--    | otherwise = Node (g,B) []
-- minimax (Node g ts)
--    | turn g == O = Node (g, minimum ps) ts'
--    | turn g == X = Node (g, maximum ps) ts'
--                    where
--                       ts' = map minimax ts
--                       ps  = [p | Node (_,p) _ <- ts']


-- bestmove :: Grid -> Player -> Grid
-- bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
--                where
--                   tree = prune depth (gametree g p)
--                   Node (_,best) ts = minimax tree


-- main :: IO ()
-- main = do hSetBuffering stdout NoBuffering
--           play empty O

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p 
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "Game ends in draw.\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of 
                      [] -> do putStrLn "ERROR: Invalid move"
                               play' g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking ..."
                   (play $! (bestmove g p)) (next p) 




{-
1. Using the function gametree, verify that there are 549,946 nodes in the complete game tree 
for a 3×3 tic-tac-toe game starting from the empty grid, and that the maximum depth of this tree is 9.


data Tree a = Node a [Tree a]
              deriving Show

-}
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]


x_win :: Grid
x_win = [[X,B,O],[B,X,O],[B,O,X]]

x_almost :: Grid
x_almost = [[X,B,O],[B,X,O],[B,O,B]]

-- count_node :: Grid -> Player -> [Int]
-- count_node g p = [1 | _ <- x]
--                   where
--                       Node (x,xs) = gametree g p 

t :: Tree Grid
t = Node [[X,B,O],[B,X,O],[B,O,B]] [Node [[X,X,O],[B,X,O],[B,O,B]] [Node [[X,X,O],[O,X,O],[B,O,B]] [Node [[X,X,O],[O,X,O],[X,O,B]] [Node [[X,X,O],[O,X,O],[X,O,O]] []],Node [[X,X,O],[O,X,O],[B,O,X]] []],Node [[X,X,O],[B,X,O],[O,O,B]] [Node [[X,X,O],[X,X,O],[O,O,B]] [Node [[X,X,O],[X,X,O],[O,O,O]] []],Node [[X,X,O],[B,X,O],[O,O,X]] []],Node [[X,X,O],[B,X,O],[B,O,O]] []],Node [[X,B,O],[X,X,O],[B,O,B]] [Node [[X,O,O],[X,X,O],[B,O,B]] [Node [[X,O,O],[X,X,O],[X,O,B]] [],Node [[X,O,O],[X,X,O],[B,O,X]] []],Node [[X,B,O],[X,X,O],[O,O,B]] [Node [[X,X,O],[X,X,O],[O,O,B]] [Node [[X,X,O],[X,X,O],[O,O,O]] []],Node [[X,B,O],[X,X,O],[O,O,X]] []],Node [[X,B,O],[X,X,O],[B,O,O]] []],Node [[X,B,O],[B,X,O],[X,O,B]] [Node [[X,O,O],[B,X,O],[X,O,B]] [Node [[X,O,O],[X,X,O],[X,O,B]] [],Node [[X,O,O],[B,X,O],[X,O,X]] []],Node [[X,B,O],[O,X,O],[X,O,B]] [Node [[X,X,O],[O,X,O],[X,O,B]] [Node [[X,X,O],[O,X,O],[X,O,O]] []],Node [[X,B,O],[O,X,O],[X,O,X]] []],Node [[X,B,O],[B,X,O],[X,O,O]] []],Node [[X,B,O],[B,X,O],[B,O,X]] []]

t1 :: Tree Grid
t1 = Node [[X,B,O],[B,X,O],[B,O,B]] []

t2 :: Tree Grid
t2 = Node [[X,B,O],[B,X,O],[B,O,X]] [Node [[X,B,O],[B,X,O],[B,O,B]] [], Node [[X,B,O],[B,X,O],[B,O,B]] []]

ts1 :: [Tree Grid]
ts1 = [Node [[X,B,O],[B,X,O],[B,O,B]] [], Node [[X,B,O],[B,X,O],[B,O,B]] []]

helper :: Tree Grid -> Grid
helper (Node a ts) = if length ts == 0 then a
                     else []

-- show_s_tree (Node a (b:bs)) = b

-- convertTreeToList :: Tree Grid -> [Grid]
-- convertTreeToList (Node a ts) | length ts == 0 = a
--                               | otherwise = [a] ++ (map convertTreeToList ts)

size_of_tree_c :: Tree Grid -> Int
size_of_tree_c (Node _ ts) | length ts == 0 = 1
                           | otherwise = 1 + sum [size_of_tree_c t | t <- ts]
                           -- | otherwise = 1 + sum (map size_of_tree_c ts)


tdepth :: Tree a -> Int
tdepth (Node _ []) = 0
tdepth (Node _ ts) = 1 + maximum (map tdepth ts)

{-

> size_of_tree_c (prune 0 (gametree empty O))
1

> size_of_tree_c (prune 1 (gametree empty O))
10

> size_of_tree_c (prune 2 (gametree empty O))
82

size_of_tree_c (prune 9 (gametree empty O))
549946


> tdepth (gametree empty O)
9
-}

{-
2. Our tic-tac-toe program always chooses the first move from the list of best moves. 
Modify the final program to choose a random move from the list of best moves, using the function 
randomRIO :: (Int,Int) -> IO Int from System.Random to generate a random integer in the given range.

-}

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts)
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']


bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g',p') _ <- ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree

-- bestmoveRand :: Grid -> Player -> IO Grid
-- bestmoveRand g p = do intR <- randomRIO (0,rand)
--                       return (minimaxList !! intR)
--                    where
--                        tree = prune depth (gametree g p)
--                        Node (_,best) ts = minimax tree
--                        minimaxList = [g' | Node (g',p') _ <- ts, p' == best]
--                        rand = ((length minimaxList) - 1)

-- main :: IO ()
-- main = do hSetBuffering stdout NoBuffering
--           randomPlay' empty O

randomPlay :: Grid -> Player -> IO ()
randomPlay g p = do cls
                    goto (1,1)
                    putGrid g
                    randomPlay' g p

-- randomPlay' :: Grid -> Player -> IO ()
-- randomPlay' g p 
--    | wins O g = putStrLn "Player O wins!\n"
--    | wins X g = putStrLn "Player X wins!\n"
--    | full g   = putStrLn "Game ends in draw.\n"
--    | p == O   = do i <- getNat (prompt p)
--                    case move g i p of
--                       [] -> do putStrLn "ERROR: Invalid move"
--                                randomPlay' g p
--                       [g'] -> randomPlay g' (next p)
--    | p == X   = do putStr "Player X is thinking ..."
--                    (randomPlay $! (bestmove g p)) (next p) 

randomPlay' :: Grid -> Player -> IO ()
randomPlay' g p 
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "Game ends in draw.\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      [] -> do putStrLn "ERROR: Invalid move"
                               play' g p
                      [g'] -> play g' (next p)
   | p == X   = do putStr "Player X is thinking ..."
                   let gs = bestmoves g p
                   n <- randomRIO (0, length gs -1)
                   play (gs !! n) (next p)


{-
3. Alternatively, modify the final program to choose a move that attempts to take the quickest route to a win, 
by calculating the depths of resulting game trees and selecting a move that results in a tree with 
the smallest depth.


-}
-- Will be done later.
{-
4. Modify the final program to:
a. let the user decide if they wish to play first or second;

Hint: Understand that X will still be played by the computer
-}
decideFirstP :: String -> IO ()
decideFirstP prompt = do putStrLn prompt
                         first <- getChar
                         if first == 'y'
                            then randomPlay' empty O
                         else if first == 'n'
                                 then randomPlay' empty X
                         else 
                            do putStrLn "ERROR: Invalid move. Press 'y' for yes. Press 'n' for no.\n"
                               decideFirstP prompt

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          decideFirstP "Do you wish to go first? Press 'y' for yes. Press 'n' for no."


{-
b. allow the length of a winning line to also be changed;

c. generate the game tree once, rather than for each move;
-}

entire_tree :: Player -> Tree Grid
entire_tree O = gametree empty O
entire_tree X = gametree empty X

{-
d. reduce the size of game tree using alpha-beta pruning.
-}

{-

-}

{-

-}

{-

-}





