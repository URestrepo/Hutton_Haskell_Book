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
             play word


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

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You Got It !!!"
               else
                  do putStrLn (match word guess)
                     play word

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
-}




























































