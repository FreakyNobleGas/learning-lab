-- Author: Nicholas Quinn
-- Description: Exploring using IO in Haskell.

import System.IO
import Data.Char(toUpper)

-- Basic Input/Output Program.
-- The '<-' Operator binds `getLine` to `inpStr`
-- putStrLn :: String -> IO()
-- getLine :: IO String
-- If a type has a IO in their return value, that is a very
-- good indicator that they will have side effects
simpleIO = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

-- The () in IO () indicates that there is no return value.
-- writefoo will print "foo" to the screen
--let writefoo = putStrLn "foo"

-- This is pure code without side effects, but is called from
-- the function `callingPureCode` which has side effects
name2reply :: String -> String
name2reply name =
    "Pleased to meet you, " ++ name ++ ".\n" ++
    "Your name contains " ++ charcount ++ " characters."
    where charcount = show (length name)

callingPureCode :: IO()
callingPureCode = do
    putStrLn "Greetings once again. What is your name?"
    -- `<-` allows you to get the input from IO
    inpStr <- getLine
    let outStr = name2reply inpStr
    putStrLn outStr

{-- Ineffective Way to Read/Write Files

-- Return is the opposite of `<-`, it is used to wrap pure functional
-- code into a IO type

main :: IO ()
main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map toUpper inpStr)
                   mainloop inh outh

--}






