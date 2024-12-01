--
-- Name: Nicholas Quinn
--
-- Description: Introduction to Data Structures following the Learning Haskell Programming course on Udemy.com
--

-- Type Synonym
type Count  = Int
processString :: String -> Count
processString = undefined


data Compass = North | East | South | West
    deriving (Eq, Ord, Enum, Show)

{-- Mechanical way of defining Show and Eq

-- Show takes an object and returns a string
instance Show Compass where
    show North = "North"
    show East = "East"
    show South = "South"
    show West = "West"

instance Eq Compass where
    North == North = True
--}

-- Example Usage: Add (Number 1) (Subtract (Number 2) (Number 3))
data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Show, Ord)

calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y)