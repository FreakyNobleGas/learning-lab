-- Name: Nicholas Quinn
--
-- Description: Exploring declaring and defining data types in Haskell.

-- Declare synonyms for types. This makes code more readable
type Name = String
type Age = Int
type DateOfBirth = Int
type PermanentAddress = String

-- Declaring dataype called People, with Person being the value constructor.
-- Name, Age, DateOfBirth, and PermanentAddress are the components.
data People = Person Name Age DateOfBirth PermanentAddress deriving Show

johnDoe = Person "John Doe" 26 01101994 "1600 Pennsylvania Ave"

-----------------------------------------------------------------------------
-- This example is taken from the book Real World Haskell

type CardHolder = String
type CardNumber = String
type Address = [String]
type CustomerID = Int

-- There are three different ways that we can declare the data type
-- BillingInfo. It can be declared by giving the Credit Card number, the
-- name of the holder, and the Address. You can also instantiate the type
-- with no arguments for cash on deliver, or just the CustomerID
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-----------------------------------------------------------------------------

customerOneBilling = Invoice 546
customerTwoBilling = CreditCard "2345" "Jane Doe" ["123 Derry Lane", "Manhattan, New York"]

-- Pattern matching, return CustomerID from Invoice. This is also known as an
-- accessor function
customerOneId (Invoice id) = id

-- Uses "_" wildcard
customerTwoId (CreditCard cardNumber _ _) = cardNumber

type ItemNum = Int
type ItemContents = [String]
type ArrivalDate = Int

-- Example of defining a datatype and accessors simultaneously
data ShippingItem = ShippingItem {
     itemNum :: ItemNum,
     itemContents :: ItemContents,
     arrivalDate :: ArrivalDate
} deriving Show

item = ShippingItem {
     itemNum = 2345,
     itemContents = ["paper", "paperclips", "ruler"],
     arrivalDate = 02022020
}


-- Algebraic Data Types, the "|" is the same as saying "Or"
data Bool = False | True

-- Enumeration
data Months = January
      | February
      | March
      | April
      | May
      | June
      | July
      | August
      | September
      | October
      | November
      | December

-- Function to calculate the sum of all elements in the list
addList (x:n) = x + addList n
addList [] = 0

-- Returns the second element of a tuple
secondElement (first, second, third) = second
