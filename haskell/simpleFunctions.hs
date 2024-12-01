-- Name: Nicholas Quinn
-- 
-- Description: Simple functions to explore Types, Variables, and Functions
--              in Haskell.

-- Returns the sum of a and b. Arguments can be either an integer or a float.
addFunction a b = a + b

-- Returns the difference of a and b 
subFunction a b = a - b

-- Returns the quotient of a and b
divFunction a b = a / b

-- Ruturns the multiplication of a and b 
multFunction a b = a * b

-- Returns boolean value representing if a is odd
isOdd a = odd a

-- Returns LT, EQ, GT when comparing a against b
compareFunction a b = compare a b

-- Creates a tuple
createTuple a b = [a] ++ [b]

-- Hello World Function
helloWorld = putStrLn "Hello World!"