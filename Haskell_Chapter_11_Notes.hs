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
x_win = [[X,B,B],[B,X,B],[B,B,X]]


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
  putStrLn . unlines . concat interleave bar . map showRow
  where bar = [replicate ((size * 4)-1) '-']





































































