-- Name: Nicholas Quinn
--
-- Description: Learning about Monads
--

{-
  (>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b

  The type (>>?) lets us chain together functions that return a Maybe value. So, if 
  the returning type is the same, you could chain Maybe functions indefinitely.

  Unfortunately, this means that if one Maybe function fails, you would have no idea where it
  stopped.


  There are 3 properties that define a Monad in Haskell:

  1) A type constructor m

  2) A function of type m a -> (a -> m b) -> m b for chaining the output of one function into the input
     of another

  3) A function of type a -> m a for injecting a normal value into the chain, i.e. it wraps a type a with the type
     constructor m.

  Example: The properties that make up the textbook's version of the Maybe monad are:

  1) Type Constuctor
  data Maybe a = Nothing
             | Just a

  2) Chaining function (>>?)
  (>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>? _ = Nothing
  Just v  >>? f = f v

  3) Injector Function Just
  inject :: a -> m a

  There is almost no rules on how chaining and injection functions should behave. Injecting is also known as 'binding'

  Monad as defined by the Prelude
  class Monad m where
    -- chain
    (>>=)  :: m a -> (a -> m b) -> m b
    -- inject
    return :: a -> m a

    `Return` returns a pure value (of type a) into a monad (of type m a)

    -- Performs chaining, but ignores value on the left
    (>>) :: m a -> m b -> m b
    a >> f = a >>= \_ -> f

    With >>=
    print "foo" >>= \_ -> print "bar"

    With >>
    print "baz" >> print "quux"

    With >>, we can omit the middle function that serves no purpose in >>= example

    Helpful Jargon:

    - "Monadic" means "pertaining to monads". A monadic type is an instance of the Monad typeclass. A monadic value has a monadic type.
    - When we say that a type "is a monad", it really means that it's an instance of the Monad typeclass.
    - "The Foo monad" is a type called Foo and it's an instance of Monad.
    - An "action" is another name for a monadic value, such as the side effect from the IO monad.

    The Maybe Monad is probably the simplest instance of Monad

    instance Monad Maybe where
    Just x >>= k  =  k x
    Nothing >>= _ =  Nothing

    Just _ >> k   =  k
    Nothing >> _  =  Nothing

    return x      =  Just x

    fail _        =  Nothing
-}

{-
-- Example of using Maybe as a Monad
import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress

variation1 person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap

-}