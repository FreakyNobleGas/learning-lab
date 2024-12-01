module Logger
    (
      Logger
    , Log
    , runLogger
    , record
    ) where

import Control.Applicative
import Control.Monad (liftM, ap)

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])

instance Functor Logger where
    fmap = liftM

instance Applicative Logger where
    pure a = Logger (a, [])
    (<*>) = ap

--(>>=) :: Logger a -> (a -> Logger b) -> Logger b
m >>= k = let 
            (a, w) = execLogger m
            n      = k a
            (b, x) = execLogger n
            in Logger (b, w ++ x)

{-
    Most of the code in this chapter does not compile, but the ideas/concepts is still represented by the code

globToRegex :: String -> Logger String
globToRegex cs =
    globToRegex' cs Prelude.>>= \ds ->
    return ('^':ds)

-- This function almost does nothing, but we still must wrap it's result in a value to call return
globToRegex' :: String -> Logger String
globToRegex' "" = return "$"

-- Calling record requires us to use >> instead of >>=
globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)

globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)

globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"

-- Example of the 'lifting' technique commonly used in monads. This lift function doesn't need to know any details
-- regarding the monad's implementation
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
            return (f i)
-}