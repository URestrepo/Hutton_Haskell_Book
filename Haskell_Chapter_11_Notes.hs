import Data.Char 
import Data.List 
import System.IO
-- Haskell
--Chapter 11
-- Unbeatable Tic-Tac-Toe

{-
Use previous concepts to develop game of tic-tac-toe.

First version allows 2 humans to play
Second version develops a computer player that uses game trees and use minimax algo
to create ensure it is unbeatable (wins or draws)


-}

------------------ Introduction ------------------

{-
Tic-tac-toe

Plays with 3 x 3 grid with players of X and O
The winner is the first player to place 3 of their marks in
horizontal, vertical, or diagonal line.

the game can end in draw if all squares are filled and previous conditions for 
winner are not present


import Data.Char 
import Data.List 
import System.IO
-}


------------------ Basic Declarations ------------------
{-
Import at the top of file
import Data.Char 
import Data.List 
import System.IO

-}

-- create grid size with intent that it can be changed to larger than 3 by 3
size :: Int
size = 3

-- Represent grid as list of lists of player values, with assumption that each inner list
-- and out list have the same length size

type Grid = [[Player]]

-- Create data type for player with B representing Blank space not occupied by other 2 types
data Player = O | B | X
              deriving (Eq, Ord, Show)

{-
Deriving clause above allows that player values support standard equality and ordering operators
and that it can be displayed on screen

************ Ordering **********
Remember ordering is determined by position in "data" declaration
Hence O < B < X
This will be useful later for minimax algo
************ Ordering **********

Next player to move will be given simply by swapping O and X
B will be included for completeness even though function should never be
applied to B value
-}

next :: Player -> Player
next O = X
next B = B
next X = O


------------------ Grid Utilities ------------------

-- Define empty grid into single list by creating row by replicating blank player and
-- then replicating empty row  to create empty grid

empty :: Grid
empty = replicate size (replicate size B)

x_win :: Grid
x_win = [[X,B,O],[B,X,O],[B,O,X]]


--Check to see if Grid is full of non-Blank
full :: Grid -> Bool
full = all (/= B) . concat

-- Recall that concat flattens list of lists to simply a list
-- We can determine whose turn it is by counting counting Os and Xs
-- and comparing number of each in flattened list

turn :: Grid -> Player
turn g = if os <= xs then O else X
            where
              os = length (filter (==O) ps)
              xs = length (filter (==X) ps)
              ps = concat g  
-- In case of empty grid, player O goes first
-- In the case of this program, player O will be human

-- Now for functions to determine winner
-- recall any function returns "True" if at least one item in the list fulfills the condition

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

-- wins_list :: Player -> Grid -> [[Player]]
-- wins_list p g = filter line (rows ++ cols ++ dias)
--            where
--               line = all (== p)
--               rows = g
--               cols = transpose g
--               dias = [diag g, diag (map reverse g)]

wins_list :: Player -> Grid -> [[Player]]
wins_list p g = (rows ++ cols ++ dias)
           where
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

{-
> map reverse x_win
[[B,B,X],[B,X,B],[X,B,B]]


> wins_list X x_win
[[X,B,B],[B,X,B],[B,B,X],[X,B,B],[B,X,B],[B,B,X],[X,X,X],[B,X,B]]

function
transpose :: [[a]] -> [[a]]
is provided in the library Data.List


What does transpose function do?

> transpose [[1,2,3],[4,5,6],[7,8,9]] 
[[1,4,7],[2,5,8],[3,6,9]]

takes a grid that is represented as a list of rows and 
reflects it about the main diagonal that runs from top-left to bottom-right, so that 
the columns become rows and vice-versa

-}

-- Function diag returns main diagonal of a grid

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]


-- the other diagonal can be obtained by first reversing each row and using diag function

-- Now to check winning player
won :: Grid -> Bool
won g = wins O g || wins X g


------------------ Displaying a Grid ------------------

{-
Put grid functions by converting each row to a list of strings using showRow,
Inserting a horizontal bar
-}

putGrid :: Grid -> IO ()
putGrid = 
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4)-1) '-']

{-
showRow is used to convert each row to list of strings,
flatten the resulting nested list structure using concat,
join strings together with newline character at each line using "unlines"

unlines is a library function
unlines :: [String] -> String

display resulting string using putStrln


showRow converts a row to list of strings, with vertical bar of length 3
between each row
-}


showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
            where
                beside = foldr1 (zipWith (++))
                bar    = replicate 3 "|"

{-
foldr1 is similar to foldr but can only be applied to non-empty lists

zipwith behaves in same manner as zip by applies given function to
each pair of values in resulting list

> showRow [O,B,X]
["   |   |   "," O |   | X ","   |   |   "]


interleave (replicate 3 "|") . map showPlayer O

-}

showPlayer :: Player -> [String]
showPlayer O = ["   "," O ","   "]
showPlayer B = ["   ","   ","   "]
showPlayer X = ["   "," X ","   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys


{-
> putGrid x_win
   |   |   
 X |   | O 
   |   |   
-----------
   |   |   
   | X | O 
   |   |   
-----------
   |   |   
   | O | X 
   |   |   


-}

------------------ Making a Move ------------------

-- We will now index the grid
{-
   |   |   
 0 | 1 | 2 
   |   |   
-----------
   |   |   
 3 | 4 | 5 
   |   |   
-----------
   |   |   
 6 | 7 | 8 
   |   |   
-}

-- Now to make index validator
-- Checks to see index is within grid range and position is blank

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

-- Now function that applies a move to grid
-- Recall a move can be invalid, so we will return list of grids
-- with convention that singleton list denotes success in applying move, 
-- and empty list denotes failure

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)

-- If valid, split list of player values in Grid at index where move is being made
-- replace the blank player value with the given player, then reform grid once again
-- The library function splitAt breaks a list into two parts at a given index,
-- Auxiliary function chop breaks a list into maximal segments of a given length

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)


------------------ Reading a Number ------------------

{-
To read a grid index from a human player, we need to define getNat
It will 
- Display prompt
- read a natural number from keyboard

It is similar to getDigit from chapter 10
-}

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt
{-
Recall function isDigit :: Char -> Bool
comes from library Data.Char, and decides if a character is a numeric digit
-}

------------------ Human vs Human ------------------

{-
Now to create two mutually exclusive recursive functions that take the current grid and player as arguments
-}

-- tictactoe :: IO ()
-- tictactoe = run empty O

-- Run simply displays the grid and invokes the 

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- clear screen --
cls :: IO ()
cls = putStr "\ESC[2J"

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p


-- second run will use guards to decide if game is finished
-- if not finished game will prompt user for a move
-- if move invalid, display error and re-prompt user
-- otherwise invoke first function with updated board and next player

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

--Function prompt
prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "




{-
To play us

> run empty O
of
> run empty X
-}
------------------ Human vs Human ------------------
{-
We will now use game trees to create computer player
Tree structure will capture all possible ways game can proceed from current grid
then use tree to decide on best move to make
-}

data Tree a = Node a [Tree a]
              deriving Show

-- In this case, Tree is given type Node that comprises a value of this type and list of subtrees
{-
Note
1.  Type is not specific to Tic Tac Toe grids but permits any type of values to be stored in the nodes
This will be important when considering minimax algorithm, which labels each grid in game tree with addition info

2. There are no constructor for leaves, as a node with an empty list of subtrees can play role of constructor
This avoids having 2 possible representations for leaves, which could complicate definition of functions on trees

3. Deriving clause ensures trees can be displayed on screen

Now to create function that builds game tree from given starting grid and player.
We use starting grid as value for root node and then recursively build game tree for each grid that results
from current player making valid move, with next player being used to continue process
-}

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]



--moves function, returns list of valid moves after checking if game finished.
-- If finished return empty list.
--Otherwise return all grids that result from player making a move
moves :: Grid -> Player -> [Grid]
moves g p
   | won g     = []
   | full g    = []
   | otherwise = concat [move g i p | i <- [0..((size^2) -1)]]

{-
> moves empty X
[[[X,B,B],
 [B,B,B],
 [B,B,B]],

[[B,X,B],
 [B,B,B],
 [B,B,B]],

[[B,B,X],
 [B,B,B],
 [B,B,B]],

[[B,B,B],
 [X,B,B],
 [B,B,B]],

[[B,B,B],
 [B,X,B],
 [B,B,B]],

[[B,B,B],
 [B,B,X],
 [B,B,B]],

[[B,B,B],
 [B,B,B],
 [X,B,B]],

[[B,B,B],
 [B,B,B],
 [B,X,B]],

[[B,B,B],
 [B,B,B],
 [B,B,X]]]
-}

------------------ Pruning the Tree ------------------
{-
Tree can become quite large. It is necessary to prune game trees to a particular depth to limit
amount of time and memory it takes to buil tree
-}

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

{-
prune 5 (gametree empty O)

produces a game tree of maximum of depth 5 from starting grid with player 0 making first move.

Note due to lazy evaluation, only as much of tree that is required by prune function will actually be produced

-}

--Constant for 3 x 3 grid entire game tree
depth :: Int 
depth = 9


------------------ Minimax Algorithm ------------------
{-
The minimax algorithm can be used to determine the next best move.
The algorithm starts by labeling every node in the tree with a player value

Manner of labeling
- leaves (nodes with no subtrees) are labeled with the winning player if there is one, and blank otherwise
- other nodes with subtrees are labeled with the minimum or maximum of the player labels from the child nodes 
one level down, depending on whose turn it is ti move: 
        - On player O's turn, we take the minimum of the child labels and on X's turn we take the maximum
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


-- Function for best moves
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree

{-
1. build game tree up to specified depth
2. apply the minimax algorithm to label the tree,
3. select a grid whose player label is the same as the root node

There is always at least 1 best move, as selecting the minimum of maximum value from a non-empty (finite) list
always results in a value in list. If more than 1 best move, select first. 
-}

------------------ Human vs Computer ------------------

{-
 we will now use main to create a compiled version
-}

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O

{-
hSetBuffering is provided in System.IO
It is used to output buffering off, which by default is on in GHC

To implement, there will be 2 mutually recursive functions, except computer player is X
-}

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
Note:
$! is used to force evaluation of best move for computer player prior to function play being invoked again
Which may cause a delay between clearing the screen and displaying grid in play while best move was calculated
under lazy evaluation


to play 
ghc -O2 Haskell_Chapter_11_Notes.hs

./Haskell_Chapter_11_Notes
-}





























