{-
 Name: Nicholas Quinn

 Description: Exercises from the book "Real World Haskell" by
 Bryan O'Sullivan, Don Stewart, and John Goerzen. Find more information
 at http://book.realworldhaskell.org/

Chapter 4 Exercises

1. Write your own “safe” definitions of the standard partial list functions, but make sure that
    yours never fail. As a hint, you might want to consider using the following types.

    -- file: ch04/ch04.exercises.hs
    safeHead :: [a] -> Maybe a
    safeTail :: [a] -> Maybe [a]
    safeLast :: [a] -> Maybe a
    safeInit :: [a] -> Maybe [a]
-}

-- safeHead takes a list of unknown type, and returns a
safeHead :: [a] -> Maybe a
-- If list is empty, then return Nothing
safeHead [] = Nothing
-- Return head of list, regardless of type
safeHead (hList:_) = Just hList

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:tList) = Just tList

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l = Just (last l)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit l = Just (init l)

{-
2. Write a function splitWith that acts similarly to words, but takes a predicate and a list of any type,
    and splits its input list on every element for which the predicate returns False.
-}

splitWith :: (a -> Bool) -> [a] -> [[a]]
-- Return empty list if  passed list is empty regardless of the predicate
splitWith _ [] = []
splitWith p xs  | null hList = splitWith p backTail
                        | null bList = [hList]
                        | otherwise = hList : splitWith p backTail
                       where
                       (hList, bList) = span p xs
                       backTail        = tail bList
