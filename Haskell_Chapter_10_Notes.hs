import Data.Char 
import Data.List 
import System.IO
-- Haskell
--Chapter 10
-- Interactive Programming

{-
This Chapter deals with how to use Haskell to write interactive programs.
Note that Haskell tries to be pure, so th


-}

------------------ The Problem ------------------

{-
First part of book assisted in writing pure functions.
Meaning taking in inputs as explicit arguments and producing outputs as explicit results

Taking in interactive responses in general forces a program to be not be pure because such
responses require side effects because it takes in additional inputs and produces additional outputs
-}

------------------ The Solution ------------------

{-
Haskell's method for handling this problem is to view interactive program as a pure function that
takes in state of world as an argument and then producing modified world as result

-}


------------------ Basic Actions ------------------
-- Now introduce some actions

-- getChar:: IO Char
-- getChar = ...



-- Function to print on screen
-- putChar :: Char -> IO ()
-- putChar c = ...

-- function that returns value v without performing any functions
-- return :: a -> IO a
-- return v = ...


------------------ Sequencing ------------------

{-
Haskell allows for sequence of IO actions to be combined into a single composite action using this form

do v1 <- a1
   v2 <- a2
   .
   .
   .
   vs <- an
   return (f v1 v2 ... vn)

Note, there are layout rules
Each action in sequence must begin in precisely same column
Like list, vi <- ai are generators because they generate variables vi

act :: IO (Char,Char)
act = do x <- getChar
      getChar
      y <- getChar
      return (x,y)





-}

act :: IO (Char,Char) 
act = do x <- getChar
         getChar
         y <- getChar 
         return (x,y)

------------------ Derived Primitives -------------------- 
{-
Using the 3 basic actions from earlier, once define certain useful actions
Many of these are provided in the Standard Prelude


***************
***************
Make sure the first element after the line do
lines up with first element after do 

For example in the function below,
the "if" is is below the "x" 
-}

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
               return []
               else
                  do xs <- getLine'
                     return (x:xs)


-- Above line reads a string of characters from keyboard
-- until terminated by the newline character
{-
> getLine'
hello world
"hello world"
Remember to hit enter to make newline ending function
-}


putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs


putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

-- Now to test using function that displays length of string from keyboard

strlen :: IO ()
strlen = do putStr' "Enter String: "
            xs <- getLine'
            putStr' "The string has "
            putStr' (show (length xs))
            putStrLn' " characters"


{-
> strlen 
Enter a string: type something 
The string has 14 characters
-}


------------------ Hangman ------------------

{-

-}
             

hangman :: IO () 
hangman = do putStrLn "Think of a word:" 
             word <- sgetLine 
             putStrLn "Try to guess it:" 
             play' word


-- sgetline reads a string of characters from keyboard like getLine
-- but echoes each character as a dash symbol to keep string secret

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)


-- getCh is used to read a single character from keyboard without it echoing
-- on the screen
--It is define by hSetEcho, a primitive type from System.IO to turn input echoing off
-- prior to reading the character, then turning it back on again

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play' :: String -> IO ()
play' word = do putStr "? "
                guess <- getLine
                if guess == word then
                   putStrLn "You Got It !!!"
                else
                   do putStrLn (match word guess)
                      play' word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]


------------------ Nim ------------------

{-
Variant of game Nim. Remove stars at end of single row. Player that empties board wins.

1 : *****
2 : ****
3 : ***
4 : **
5 : *

Unlike Hangman, this game will be implemented in bottom-up manner, starting by defining utility functions


Assume we represent player as 1 or 2
-}

next :: Int -> Int
next 1 = 2
next 2 = 1

-- Board will be represented as list comprising number of stars remaining in each row
-- Initial list will be [5,4,3,2,1]. Game finished with no stars

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (==0)

-- a move will be defined by specifying row and number of stars to remove
-- move valid if row has that number of stars at least to remove

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

-- Reason we subtract row by 1, list indexing starts at 0
{-
> valid initial 4 3
True

> valid initial 1 3
False
-}

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
   where update r n = if r == row then n-num else n


-- Function to display a row

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))



-- Function to put all rows visibly
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

-- function to display a prompt and read characters from keyboard
-- If digit, corresponding Int is returned value
-- otherwise error is displayed and user is re-prompted to enter digit

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid Digit"
                           getDigit prompt

{-
Function digitToInt :: Char -> Int converts a digit to an integer
Can be imported using "import Data.Char"

Function to move to newline
-}

newline :: IO ()
newline = putChar '\n'


-- Now main game loop

play :: Board -> Int -> IO ()
play board player = 
   do newline
      putBoard board
      if finished board then
         do newline
            putStr "Player "
            putStr (show (next player))
            putStrLn " wins !!!"
      else
         do newline
            putStr "Player "
            putStrLn (show player)
            row <- getDigit "Enter a row number: "
            num <- getDigit "Stars to remove : "
            if valid board row num then
               play (move board row num) (next player)
            else
               do newline
                  putStrLn "ERROR: Invalid Move"
                  play board player


nim :: IO ()
nim = play initial 1


{-
When implementing states, it is good practice to separate pure functions from impure ones
-}



------------------ Life ------------------
{-
Now implement game of life
A simple evolutionary system game based on cells, and is played on a two-dimensional board.
Each square on the board is either empty, or contains a single living cell.

Each square has 8 neighbors assuming board wraps around.

Cells are created based on a next generation
- a living cell survives if it has precisely two or three neighboring squares that contain living cells, and 
- an empty square gives birth to a living cell if it has precisely three neighbors that contain living cells, and 
- remains empty otherwise.

It is modified by 2 Int values that specify size of board

-}
cls :: IO () 
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")


width :: Int
width = 10

height :: Int
height = 10


type Board' = [Pos]

glider :: Board'
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

{-
The library function sequence_ :: [IO a] -> IO ()
performs a list of actions in sequence, discarding their 
result values and returning no result.
-}

showcells :: Board' -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board' -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board' -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1),
                          (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board' -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board' -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board' -> [Pos]
births b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveneighbs b (x,y) == 3]


{-
The next generation of a board can be produced by appending 
the list of survivors and the list of new births.
-}

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)


nextgen :: Board' -> Board'
nextgen b = survivors b ++ births b

life :: Board' -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)

{- slow the game down to a reasonable speed by 
performing a given number of dummy actions -}

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]


{-
To run game of life implementation:
> life glider
To stop process: ctrl + z
-}


























