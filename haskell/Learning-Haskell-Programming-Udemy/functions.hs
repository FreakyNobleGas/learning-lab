--
-- Name: Nicholas Quinn
--
-- Description: Introduction to Functions following the Learning Haskell Programming course on Udemy.com

main :: IO()
main = putStrLn (greet "World")

greeting = "Howdy"
greet who = greeting ++ ", " ++ who

-- Takes 2 Ints and returns an Int. In GHCI, an interesting behavior is that if you say `add 1`, you will get a 
-- no instance error for `(Show (Int -> Int))` so there is a function that exists. You can then say in GHCI:
--
-- add1 = add 1
-- add1 2
-- The result will be `3`. 
add :: Int -> Int -> Int
add a b = a + b


-- The following series of add functions, shows the many ways we can define the add function
add1 :: Int -> Int -> Int
add1 a b = a + b

add2 :: Int -> Int -> Int
add2 a b = (+) a b

add3 :: Int -> Int -> Int
add3 = (+)

-- With this final definition, we see that add is really a synonym for the add operator
add4 :: Num a => a -> a -> a
add4 = (+)