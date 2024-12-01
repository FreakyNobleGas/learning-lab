-- Name: Nicholas Quinn
--
-- Description: Reads a positive integer, n, from the standard input and prints
--              "Hello World", n number of times.

-- If n is greater than zero, then print "Hello World", and recursively call
-- function with n - 1.
helloWorld n = if n > 0
               then do putStrLn "Hello World"
                       helloWorld (n-1)
               else return ()

main :: IO()
main = do
    n <- readLn :: IO Int
    helloWorld n
