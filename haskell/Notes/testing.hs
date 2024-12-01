-- Name: Nicholas Quinn
--
-- Description: Learning about testing and Quality Assurance
--

{-
    Haskell has many tools designed for testing and quality assurance. An example of a testing
    tool that is built into haskell is the expressive type-system. Haskell's expressive type-system
    enforces statically, and is caught in compilation.

    Open source testing libraries include HUnit and QuickCheck.

    QuickCheck Documentation: https://hackage.haskell.org/package/QuickCheck
-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.List
import System.Random

-- Function that will be tested using QuickCheck
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

-- idempotency: Applying a function twice has the same result as applying it only once
prop_idempotent xs = qsort (qsort xs) == qsort xs

{-- QuickCheck Testing Cases. This is inefficient because we have to write these test cases by hand. If only
    there was a way to automate this process...
prop_idempotent []
prop_idempotent [1,1,1,1]
prop_idempotent [1..100]
prop_idempotent [1,5,2,1,2,0,9]


QuickCheck library comes with a set of data generators that uses the `Arbitrary` typeclass to present a uniform interface
to create pseudo-random data.

This command generates a random list of 10 booleans (Though this command is old, and does not work with my version of QuickCheck)
generate 10 (System.Random.mkStdGen 2) arbitrary :: [Bool]

QuickCheck has a function called `quickCheck` that generates test data based on your choosing. (Including nothing ())
Usage:

    :type quickCheck -- returns `quickCheck :: Testable prop => prop -> IO ()`

    -- This function call generates 100 tests of different [Integer] combinations
    quickCheck (prop_idempotent :: [Integer] -> Bool) -- returns `+++ OK, passed 100 tests`

    -- This is identical to quickCheck, but prints out each test case
    verboseCheck (prop_idempotent :: [Integer] -> Bool)
--}

-- The first element of qsort should be the smallest. Though, if we attempt to test this with
-- quickCheck (prop_minimum :: [Integer] -> Bool), we will see that it fails on empty list
prop_minimum xs         = head (qsort xs) == minimum xs

-- Better version after testing
-- quickCheck (prop_minimum' :: [Integer] -> Property) -- Note: The return type is now Property
prop_minimum' xs         = not (null xs) ==> head (qsort xs) == minimum xs

-- Order Output
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

-- Confirm that the output is a permutation of the input
prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

-- Confirm last element is the largest
prop_maximum xs         =
    not (null xs) ==>
        last (qsort xs) == maximum xs

-- The minimum of a list should be the same minimum returned by qsort
prop_append xs ys       =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

--
-- Testing against a model
--

-- This function tests the implementation of qsort against a sort function already defined for us in the prelude.
-- If these functions produce the same output, then we gain confidence that qsort is behaving as intended.
prop_sort_model xs      = sort xs == qsort xs

--
-- Case Study - Specifying a Pretty Printer
--

-- Algebraic Data Type that represents well-formed documents
data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)

{- 

-- The following code snippets are a combination of actual code and 'hypothetical code', so I decided
-- to leave everything commented out.

-- Class to generate data for each of our types in the pretty printer for QuickCheck
class Arbitrary a where
  arbitrary   :: Gen a
  elements    :: [a] -> Gen a
  --choose   :: Random a => (a, a) -> Gen a
  --oneof    :: [Gen a] -> Gen a

-- Simple data type
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)

-- Simple definition of instance
instance Arbitrary Ternary where
  arbitrary     = elements [Yes, No, Unknown]

-- Specifically generate our own values for Gen to use
instance Arbitrary Ternary where
  arbitrary     = do
      n <- choose (0, 2) :: Gen Int
      return $ case n of
                    0 -> Yes
                    1 -> No
                    _ -> Unknown

-- Instance for Sums/Products
instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (x, y)

-- Generate our own random characters for QuickCheck
instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")

-- Write instances of document
instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1,6) :: Gen Int
        case n of
             1 -> return Empty

             2 -> do x <- arbitrary
                     return (Char x)

             3 -> do x <- arbitrary
                     return (Text x)

             4 -> return Line

             5 -> do x <- arbitrary
                     y <- arbitrary
                     return (Concat x y)

             6 -> do x <- arbitrary
                     y <- arbitrary
                     return (Union x y)

-- Gives the ability to select various types of generators
instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]
-}