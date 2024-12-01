-- Name: Nicholas Quinn
--
-- Description: Given a number n, print each of the elements in the array, arr,
--              n number of times

f :: Int -> [Int] -> [Int]

-- Attempting to print the first element of the array
f n arr = let x = head arr in print x


-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
