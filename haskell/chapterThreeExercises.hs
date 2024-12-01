{-
 Name: Nicholas Quinn

 Description: Exercises from the book "Real World Haskell" by
 Bryan O'Sullivan, Don Stewart, and John Goerzen. Find more information
 at http://book.realworldhaskell.org/
-}

-- Import Libraries
import Data.List
import Data.Char

{-
 Chapter 3 Exercises
-}

-- Exercise 1: Write a function that computes the number
-- of elements in a list. To test it, ensure that it gives
-- the same answers as the standard length function.

-- List of 5 colors
colors = ["green", "blue", "orange", "yellow", "red"]

-- Function that recursively computes all the elements in
-- a given list
computeLengthOfList (x:xr) = 1 + computeLengthOfList xr
computeLengthOfList [] = 0

answer31 = let x = computeLengthOfList colors in
         if x == length colors
         then putStrLn "Length is Correct!"
         else putStrLn "Length is incorrect!"

-- Exercise 2: Add a type signature for your function to
-- your source file. To test it, load the sourcefile into
-- ghci again.

data ListType = StringList [String]
                | IntList [Int]
                | CharList [Char]
                | BoolList [Bool]

-- Exercise 3: Write a function that computes the mean of a
-- list, i.e. the sum of all elements in the list divided by
-- it's length. (You may need to use fromIntegral function to
-- convert the length of the list from an integer into a floating
-- point number.)

listOfNum = [12.0,42.0,3.0,89.0,2.0,4.0]

calculateSumOfList (x:xl) = x + calculateSumOfList xl
calculateSumOfList [] = 0

calculateMeanOfList (x:xl) = let sum = calculateSumOfList (x:xl) in
                             let l = genericLength (x:xl) in
                             sum / l
