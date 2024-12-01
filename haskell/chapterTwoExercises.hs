{-
 Name: Nicholas Quinn

 Description: Exercises from the book "Real World Haskell" by
 Bryan O'Sullivan, Don Stewart, and John Goerzen. Find more information
 at http://book.realworldhaskell.org/

Chapter Two Exercises

1. What are the types of the following expressions?
-}

import Data.Typeable
import Data.List (reverse)

t1 = False -- Bool
t2 = (["foo", "bar"], 'a') -- ([[Char]],Char)
t3 = [(True, []), (False, [['a']])] -- [(Bool,[[Char]])]

printType t = print (typeOf t)

{-
2. Haskell provides a standard function, last :: [a] -> a, that returns the last element of a list.
   From reading the type alone, what are the possible valid behaviours (omitting crashes and
   infinite loops) that this function could have? What are a few things that this function clearly
  cannot do?

  Valid Behaviors:
   -  If passed last a list of 1 or more elements, then last will return the element of it's type
  Invalid Behaviors:
    - Passing last an empty list will result in an exception

  3. Write a function lastButOne, that returns the element before the last.
-}

lastButOne :: [a] -> a
lastButOne (x:xs) = head (drop (length (x:xs) - 2) (x:xs))

-- Returns the second to last element of a list if there is more than 2 elements
lastButOne2 :: [a] -> Maybe a
lastButOne2 x = case x of 
            [] -> Nothing
            [x] -> Nothing
            (x:xs) -> Just (head (drop (length (x:xs) - 2) (x:xs)))



