--
-- Name: Nicholas Quinn
--
-- Description: Introduction to Pattern Matching following the Learning Haskell Programming course on Udemy.com
--

data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Show, Ord)

-- Takes an Expression data type and returns an Int
calculate :: Expression -> Int
-- Example: calculate (Number 1)
calculate (Number x) = x
-- Example: calculate (Add (Number 2) (Number 3))
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y)

newHead :: [a] -> a
-- Empty list case
newHead [] = error "Empty List!"
-- The : operator adds to a list, so we are saying 'head : [...]'
newHead (x:xs) = x

newTail :: [a] -> [a]
newTail [] = error "Empty List!"
newTail (x:xs) = xs
