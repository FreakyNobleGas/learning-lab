--
-- These examples are taken/derived from the textbook "Real World Haskell"
--

{-
A module in Haskell let's us determine which names inside a module are accessible from other modules. A source file
begins with a module declaration. This must precede all other functions in the source file. The name of the module
"SimpleJSON" must always start with a capital letter. Inside the modules is a list of exports, which can be visible to
other files. The ".." notation indicates that we are exporting both the type and all of it's constructors.

To compile this source file, use "ghc -c learnJSON.hs". The -c option tells the compiler to only create object code.
-}
module LearnJSON
    (
      JValue(..)
    , getString
    , getBool
    , getObject
    , getArray
    , isNull
    ) where

{-
 Algebraic data type representing all JSON types

 Basic Data Types
   - Supports strings, numbers, booleans, and null

 Compound Data Types
   - Array: Ordered sequence of values.
     Ex: [-3.14, true, null, "a string"]
   - Object Unordered collection of names/value pairs
     Ex: {"numbers": [1,2,3,4,5], "useful": false}
-}
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

{-
 Function that uses pattern matching that will extract a string from the JValue type
 Example Usage:
 test = JString "Hello!"
 getString test
 This will return "Just Hello!"

 Further Notes: The just keyword will return a string if a value exists, otherwise, it will
 match the last pattern match and return nothing.
 https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell/18809252#18809252
-}
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getNumber :: JValue -> Maybe Double
getNumber (JNumber n) = Just n
getNumber _           = Nothing

-- For compatibility
getDouble = getNumber

getObject :: JValue -> Maybe [(String, JValue)]
getObject js = case js of
               JObject xs -> Just xs
               _          -> Nothing


getArray :: JValue -> Maybe [JValue]
getArray js = case js of
              JArray xs -> Just xs
              _         -> Nothing

-- For compatibility
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

isNull :: JValue -> Bool
isNull JNull = True
isNull _     = False
