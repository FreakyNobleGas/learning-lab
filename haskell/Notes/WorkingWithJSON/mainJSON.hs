--
-- These examples are taken/derived from the textbook "Real World Haskell"
--
-- Usage: Compile with the command "ghc -o learnjson mainJSON.hs" after compiling
-- learnJSON.hs as an object file.
module Main (main) where

-- Imports module defined in learnJSON.hs
import LearnJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
