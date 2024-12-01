--
-- These examples are taken/derived from the textbook "Real World Haskell"
--

module PutJSON where

import Data.List (intercalate)
import LearnJSON

-- Generate JSON values
renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)

-- Function that performs IO for renderJValue. This is also an example of seperating pure functional
-- code from code with side effects
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
