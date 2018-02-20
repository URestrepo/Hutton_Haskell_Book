import Data.Char 
import Data.List 
import System.IO

{-
10.10 Exercises

1. Redefine putStr :: String -> IO () using a list comprehension and
the library function sequence_ :: [IO a] -> IO ().

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

sequence_ :: [IO a] -> IO ()                    
-}
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]



{-
2. Using recursion, define a version of putBoard :: Board -> IO () that
displays nim boards of any size, rather than being specific to boards 
with just five rows of stars. 


Hint: first define an auxiliary function that takes the current row number as an additional argument.
-}
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
   where update r n = if r == row then n-num else n


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid Digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'

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

-- Function to display a row

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- putBoard :: Board -> IO ()
-- putBoard [a,b,c,d,e] = do putRow 1 a
--                           putRow 2 b
--                           putRow 3 c
--                           putRow 4 d
--                           putRow 5 e

putBoard :: Board -> IO ()
putBoard xs = board_helper' 1 xs

-- Hint: define an auxiliary function that takes the current row number as an additional argument.
board_helper' :: Int -> Board -> IO ()
board_helper' row []     = do newline 
board_helper' row (x:xs) = do putRow row x 
                              board_helper' (row + 1) xs


{-
3. In a similar manner to the first exercise, redefine the generalized version of putBoard using 
a list comprehension and sequence_.


putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

putBoard :: Board -> IO ()
putBoard xs = board_helper' 1 xs

board_helper' :: Int -> Board -> IO ()
board_helper' row []     = do newline 
board_helper' row (x:xs) = do putRow row x 
                              board_helper' (row + 1) xs
-}
putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [ putRow row x | (row,x) <- zip [1..] xs]

{-
4. Define an action adder :: IO () that reads a given number of integers from the keyboard, 
one per line, and displays their sum. For example:

> adder How many numbers? 5 
1 
3 
5 
7 
9 
The total is 25

Hint: start by defining an auxiliary function that takes the current total and 
how many numbers remain to be read as arguments. 
You will also likely need to use the library functions read and show.
-}
adder :: IO ()
adder = do newline
           num <- getDigit "How many numbers? "
           helpAdder num 0

helpAdder :: Int -> Int -> IO ()
helpAdder 0 total     = do putStr "The total is "
                           putStr (show total)
                           newline
helpAdder times total = do num <- getDigit ""
                           helpAdder (times - 1) (total + num)




-- helpAdder :: IO ()
-- helpAdder = do putStrLn "How many numbers? "
--                x <- getChar
--                newline
--                putChar x
                     -- if isDigit x then
                        -- return (digitToInt x)
                     -- else
                        -- do putStrLn "ERROR: Invalid Digit"
                           -- helpAdder prompt

{-
5. Redefine adder using the function sequence :: [IO a] -> IO [a] that

performs a list of actions and returns a list of the resulting values.

Probably needs "replicate" 
> :t replicate
replicate :: Int -> a -> [a]

So from my understanding, one can use sequence to perform actions that return lists
then it concats the lists inside to make one list

sequence (replicate 5 (getDigit ""))
-}


adder_helper' :: Int -> IO [Int]
adder_helper' a = sequence (replicate a (getDigit ""))

adder' :: IO ()
adder' = do n <- getDigit "How many numbers?: "
            if n < 1 then
               do putStrLn "ERROR: there must be more than 0 numbers!"
                  adder'
            else
               do nums <- adder_helper' n
                  putStr "the total is "
                  putStrLn (show $ sum nums) -- convert Int to String with show


-- adder'' :: IO ()
-- adder'' = do num <- getDigit "How many numbers? "
--              putStr "The total is "
--              putStr ( show $ sum (sequence (replicate num (getDigit "")))

adder'' :: IO ()
adder'' = do num <- getDigit "How many numbers? "
             nums <- sequence (replicate num (getDigit ""))
             putStr "The total is "
             putStrLn (show $ sum nums)
             -- where  = [1,2,3,4,5]



{-
6. Using getCh, define an action readLine :: IO String that behaves in
the same way as getLine, except that it also permits the delete key to be used to remove characters. 
Hint: the delete character is ’\DEL’, and the control character for moving the cursor back one space is ’\b’.

Probably need to create helper function so that function is recursive so that 
one can use the list that has already been entered

Also, a remove function seems necessary to take care of the init exceptions of say en empty list
-}


{-
getChar vs GetCh

> getChar
a'a'
> getCh
'a'

-}
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
               return []
               else
                  do xs <- getLine'
                     return (x:xs)


-- recursive function to does 1 of 3 things
-- if hit enter, add put newline in terminal and add string so far added, end recursion
-- if hit delete, go back, put space, and go back again.
-- if not above actions, add character to list
getLineHelp :: String -> IO String
getLineHelp xs = do x <- getCh
                    if x == '\n' then
                       do putChar x
                          return xs
                    else if x == '\DEL' then
                            do putChar '\b'
                               putChar ' '
                               putChar '\b'
                               getLineHelp (removeChar xs)
                    else
                            do putChar x
                               getLineHelp (xs ++ [x])

-- Function to start getLine'' without need to enter empty list
getLine'' :: IO String
getLine'' = getLineHelp []



removeChar :: [Char] -> [Char]
removeChar [] = []
removeChar xs = init xs



{-
> removeChar "brainy"
"brain"


{-

-}
-}

{-
Ask Charles, why the need for auxiliary function that takes the current total and 
how many numbers remain to be read as arguments

How does one use the "enter" button to go to next argument?

When is the return function useful in your opinion




-}



return' :: a -> IO a 
return' v = ...








