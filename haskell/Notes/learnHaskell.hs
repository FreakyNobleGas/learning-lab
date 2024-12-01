-- Single Line Comments

{-
Multi Line comments

Example of Compiler Output
sqrt :: Floating a=> a -> a
-}

-- Import modules (Note: The prelude library is imported by default)
import Data.List
import Data.Map
import System.IO

test = Prelude.foldl (+) 0 [5,3,6]

-- Int Min = -2^63 Max = 2^63
maxInt = maxBound :: Int
minInt = minBound :: Int

-- Haskell has Int, Double, Float, Bool, Char
--             Tuple
bigFloat = 3.999999 + 1.222222

-- Built-In Math Functions
calculateSum = sum [1..1000]
-- Pre-fix operator
modEx = mod 5 4
-- In-Fix Operator
modEx2 = 5 `mod` 4

negNumEx = 5 + (-4)

-- Mappings
-- Equivilant to adding 1 to each element of the list
--map(\x -> x + 1)[1..5]

-- Folding
-- foldl folds list from the left and foldr folds from the
-- right. This would make a difference in an operation like
-- subtraction.
-- Equivilant of ((0 + 5) + 3) + 6
