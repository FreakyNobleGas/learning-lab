-- Name: Nicholas Quinn
--
-- Description: Learning Typeclasses in Haskell

-- Example function of implied types in Haskell:
-- If we do not tell Haskell the type of values that should
-- be expected, then we will get unpredictable behavior. For
-- example, dividesEvenly 2 4 will return true as expected, but
-- dividesEvenly 2 5 will also return true, because 5 / 2 will return
-- a float.

-- "::" is read as "has type of"
dividesEvenly :: Int -> Int -> Bool

-- This function will fail because '/' is of class fractional
-- dividesEvenly x y = (y / x) * x == y

dividesEvenly x y = (y `div` x) * x == y

-- Defining a simple typeclass called BasicEq. An instance of
-- this type class is any type that implements isEqual or any
-- functions in BasicEq.
--
-- *Main> :type isEqual
-- isEqual :: BasicEq a => a -> a -> Bool

class BasicEq a where
    -- isEqual takes two arguments and returns a bool
    isEqual :: a -> a -> Bool

-- Defining isEqual
instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

-- Type class with two functions, both will now have to be implemented
class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool

-- Since isEqual3 equals the negated result of isNotEqual, we only have to
-- implement one function, otherwise, it'll be an endless loop
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)

data Color = Red | Green | Blue

instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False

{--
These are important built-in typeclasses included in Prelude.

Show converts values into a string

show 1 will return "1"
show [1,2,3] will return "[1,2,3]"

Read is the opposite of show, it takes a string and parses the
input. Read is not widely used except for simpler tasks, instead
Parsec is used.
--}

-- Define Show for the custom type Color
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

-- Example of using read
readExample = do
        putStrLn "Please enter a Double:"
        inpStr <- getLine
        let inpDouble = (read inpStr)::Double
        putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

--
-- Automatic Derivation 
--
data CannotShow = CannotShow
                deriving (Show)

-- will not compile, since CannotShow is not an instance of Show
data CannotDeriveShow = CannotDeriveShow CannotShow
                        deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
                 deriving (Show)

--
-- Helpful Errors
--
-- While the maybe type can be used for error handling, returning "nothing" is 
-- not always useful
data Maybe a = Nothing
             | Just a
               deriving (Eq, Ord, Read, Show)

-- In the either type, the "Left" constructor is used when something bad happens
data Either a b = Left a
                | Right b
                  deriving (Eq, Ord, Read, Show)

-- newtype keyword
data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)

-- There are some restrictions on creating a newtype

-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- ok: exactly one field
newtype Okay = ExactlyOne Int

-- ok: type parameters are no problem
-- newtype Param a b = Param (Either a b) Commented out due to conflict with above datatype

-- ok: record syntax is fine
newtype Record = Record {
      getInt :: Int
    }

-- bad: no fields
--newtype TooFew = TooFew

-- bad: more than one field
--newtype TooManyFields = Fields Int Int

-- bad: more than one constructor
--newtype TooManyCtors = Bad Int
--                     | Worse Int

-- Using the keyword "type" acts as a synoynm for a name for a data type,
-- and can therefore be used interchangably. 