module Main where

import Lib

--
-- Main Driver
--
main :: IO()
main = do 
  -- Lazy I/O
  contents <- readFile "listOfPokemon.csv"

  -- Convert String to [String]
  let a = lines contents

  -- See if IO [String] can be passed to 'pure' function
  let b = parseComma a

  let c = generateAllPokemon b

  print c