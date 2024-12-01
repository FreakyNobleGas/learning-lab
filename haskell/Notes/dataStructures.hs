-- Author: Nicholas Quinn
-- Description: Exploring Data Stuctures in Haskell

import Data.List
import System.IO
import Control.Monad(when)
import System.Exit
import System.Environment(getArgs)
-- Import as qualified because some functions in Data.Map are the same as functions included in the Prelude
import qualified Data.Map as Map

-- Association Lists and Maps are really great for dealing with object typed data that has key-value pairs

--
-- Functions to generate a Map that represents an association list as a map. Maps are very similar to association lists,
-- but offer better performance. Maps give us the same abilility as hash maps in other languages. Internally, they are represented
-- as a balanced binary tree. 
--
al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

{- | Create a map representation of 'al' by converting the association
-  list using Map.fromList -}
mapFromAL =
    Map.fromList al

{- | Create a map representation of 'al' by doing a fold -}
-- This function specifically uses Map.insert to build a map which returns a copy of the input data in map form
mapFold =
    foldl (\map (k, v) -> Map.insert k v map) Map.empty al

{- | Manually create a map with the elements of 'al' in it -}
-- Returns a Map.Map Integer [Char]
mapManual =
    Map.insert 2 "two" . 
    Map.insert 4 "four" .
    Map.insert 1 "one" .
    Map.insert 3 "three" $ Map.empty

--
--
--

{- | Our usual CustomColor type to play with -}
data CustomColor =
  CustomColor {red :: Int,
               green :: Int,
               blue :: Int}
  deriving (Eq, Show, Read)

{- | A new type that stores a name and a function.

The function takes an Int, applies some computation to it, and returns
an Int along with a CustomColor -}
data FuncRec =
    FuncRec {name :: String,
             colorCalc :: Int -> (CustomColor, Int)}

plus5func color x = (color, x + 5)

purple = CustomColor 255 0 255

plus5 = FuncRec {name = "plus5", colorCalc = plus5func purple}
always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}

--
--
--

{--
-- This function needs to be executed on a Linux machine to work properly, but it demostrates the process of finding
-- a username with the UID key.

-- Simple function that can look up a list of tuples given a key
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((thiskey,thisval):rest) =
    if key == thiskey
       then Just thisval
       else myLookup key rest

main = do
    -- Load the command-line arguments
    args <- getArgs

    -- If we don't have the right amount of args, give an error and abort
    when (length args /= 2) $ do
        putStrLn "Syntax: passwd-al filename uid"
        exitFailure

    -- Read the file lazily
    content <- readFile (args !! 0)

    -- Compute the username in pure code
    let username = findByUID content (read (args !! 1))

    -- Display the result
    case username of 
         Just x -> putStrLn x
         Nothing -> putStrLn "Could not find that UID"

-- Given the entire input and a UID, see if we can find a username.
findByUID :: String -> Integer -> Maybe String
findByUID content uid =
    let al = map parseline . lines $ content
        in lookup uid al

-- Convert a colon-separated line into fields
parseline :: String -> (Integer, String)
parseline input =
    let fields = split ':' input
        in (read (fields !! 2), fields !! 0)

{- | Takes a delimiter and a list.  Break up the list based on the
-  delimiter. -}
split :: Eq a => a -> [a] -> [[a]]

-- If the input is empty, the result is a list of empty lists.
split _ [] = [[]]
split delim str =
    let -- Find the part of the list before delim and put it in "before".
        -- The rest of the list, including the leading delim, goes
        -- in "remainder".
        (before, remainder) = span (/= delim) str
        in
        before : case remainder of
                      [] -> []
                      x -> -- If there is more data to process,
                           -- call split recursively to process it
                           split delim (tail x)

--}