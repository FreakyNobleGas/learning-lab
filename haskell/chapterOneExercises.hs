{-
 Name: Nicholas Quinn

 Description: Exercises from the book "Real World Haskell" by
 Bryan O'Sullivan, Don Stewart, and John Goerzen. Find more information
 at http://book.realworldhaskell.org/

Chapter One Exercises

1. Enter the following expressions into ghci. What are their types?
-}

import Data.Dynamic
import Data.Typeable

t1 = 5 + 8 -- Integer
t2 = 3 * 5 + 8 -- Integer
t3 = 2 + 4 -- Integer
t4 = (+) 2 4 -- Integer
t5 = sqrt 16 -- Double
t6 = succ 6 -- Integer
t7 = succ 7 -- Integer
t8 = pred 9 -- Integer
t9 = pred 8 -- Integer
t10 = sin (pi / 2) -- Double
t11 = truncate pi -- Integer
t12 = round 3.5 -- Integer
t13 = round 3.4 -- Integer
t14 = floor 3.7 -- Integer
t15 = ceiling 3.3 -- Integer

printType t = print (typeOf t)

{-
  2. From ghci, type :? to print some help. Define a variable, such as let x = 1, then type :show
   bindings. What do you see?

   :? in GHCi shows commands for debugging, changing settings, commands for displaying information,
   and etc.

   Prelude> :show
   options currently set: none.
   base language is: Haskell2010
   with the following modifiers:
   -XNoDatatypeContexts
   -XNondecreasingIndentation
   GHCi-specific dynamic flag settings:
   other dynamic, non-language, flag settings:
   -fignore-optim-changes
   -fignore-hpc-changes
   -fimplicit-import-qualified
   warning settings:
   Prelude>


   3. The words function counts the number of words in a string. Modify the WC.hs example to count
      the number of words in a file.

      -- file: ch01/WC.hs
      -- lines beginning with "--" are comments.

      main = interact wordCount
               where wordCount input = show (length (lines input)) ++ "\n"
-}

main = interact wordCount
         where wordCount input = show (length ( words (input))) ++ "\n"

{-
    4. Modify the WC.hs example again, to print the number of characters in a file.
-}

main = interact wordCount
        where wordCount input = show (length (input)) ++ "\n"
