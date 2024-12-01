-- Name: Nicholas Quinn
--
-- Description: This file contains classes and functions that allow for the
-- creation of a unique Pokemon.
--
----------------------------------------------------------------------------

import System.IO
import Data.List (lines)
import Data.List.Split
import Text.XML.HXT.DOM.Util

---------------------------- Classes and Data Types ------------------------
--
-- This class allows each unique Pokemon to access it's various traits
-- and/or set them
--
class UniquePokemon p where    
    getName :: p -> String
    getIndex :: p -> Int 
    getType :: p -> PokeType

--
-- Definition of methods defined in UniquePokemon class that retrieve information
-- about a specific Pokemon
--
instance UniquePokemon Pokemon where    
    getName p = name p
    getIndex p = index p
    getType p = pokeType p

--
-- Pokemon Data Type
--
data Pokemon = Pokemon {
    name :: String,
    index :: Int,
    pokeType :: PokeType
} deriving (Show)

--
-- Alias for List of Pokemon
--
type PokemonDict = [Pokemon]

--
-- All 1st Generation Pokemon Types
--
data PokeType = Normal | Fire | Water | Electric | Grass | Ice |
                 Fighting | Poison | Ground | Flying | Psychic |
                 Bug | Rock | Ghost | Dragon | Dark | Steel | Fairy
                deriving (Eq, Ord, Show, Read, Bounded, Enum)

--
-- Declare each type of effectiveness
--
data Effectiveness = SuperEffective | NormalEffectiveness | NotVeryEffective | NoEffect

--
-- Define show for various effectiveness
--
instance Show Effectiveness where
    show SuperEffective = "Super Effective!"
    show NormalEffectiveness = "Normal Effectiveness"
    show NotVeryEffective = "Not Very Effective!"
    show NoEffect = "No Effect"

---------------------------- Helper Functions ------------------------------
--
-- Match a string to it's Poketype
--
findPokeType :: String -> PokeType
findPokeType p = case p of
                      "Normal"   -> Normal
                      "Fire"     -> Fire
                      "Water"    -> Water
                      "Electric" -> Electric
                      "Grass"    -> Grass
                      "Ice"      -> Ice
                      "Fighting" -> Fighting
                      "Poison"   -> Poison
                      "Ground"   -> Ground
                      "Flying"   -> Flying
                      "Psychic"  -> Psychic
                      "Bug"      -> Bug
                      "Rock"     -> Rock
                      "Ghost"    -> Ghost
                      "Dragon"   -> Dragon
                      "Dark"     -> Dark
                      "Steel"    -> Steel
                      "Fairy"    -> Fairy
                      _          -> error "PokeType does not exist!"

--
-- Filter list of Pokemon based on Pokemon's information
--
filterByType :: PokemonDict -> PokeType -> PokemonDict
filterByType dict t = filter determineType dict 
    where determineType p = t == (getType p)

filterByName :: PokemonDict -> String -> PokemonDict
filterByName dict n = filter determineName dict 
    where determineName p = n == (getName p)

filterByIndex :: PokemonDict -> Int -> PokemonDict
filterByIndex dict i = filter determineIndex dict 
    where determineIndex p = i == (getIndex p)
                                 
--
-- Convert list of strings to useable data so that we can create a Pokemon object
--
generatePokemonDetails :: [String] -> (String, Int, PokeType)
generatePokemonDetails p = 
    let name     = p !! 1
        index    = p !! 0 
        pokeType = p !! 2
    in (name, (decimalStringToInt index), (findPokeType pokeType))

--
-- Construct a new Pokemon object
--
generatePokemon :: (String, Int, PokeType) -> Pokemon
generatePokemon (newName, newIndex, newPokeType) = 
    Pokemon {name = newName, index = newIndex, pokeType = newPokeType}

--
-- Go through each of the Pokemon strings and resolve strings into their appropriate type
-- and return a list of Pokemon objects
--
generateAllPokemon :: [[String]] -> PokemonDict
generateAllPokemon p = 
    let all = map generatePokemonDetails p
    in map generatePokemon all
                    
--
-- Function that opens a file stream to read in data from "ListOfPokemon.csv" into
-- an array with each Pokemon's characteristics seperated by a comma.
--
parseComma :: [String] -> [[String]]
parseComma p = map (splitOn ",") p

--
-- Print each Pokemon individually
--
printEachPokemon :: PokemonDict -> IO()
printEachPokemon p = mapM_ print p

--
-- Print each PokeType
--
pokeTypeString :: [String]
pokeTypeString = ["Normal", "Fire", "Water", "Electric", "Grass", "Ice",
                  "Fighting", "Poison", "Ground", "Flying", "Psychic",
                  "Bug", "Rock", "Ghost", "Dragon", "Dark", "Steel", "Fairy"]

printEachPokemonType :: IO()
printEachPokemonType = mapM_ putStrLn pokeTypeString

---------------------------- Main Driver -----------------------------------
--
--
--
main :: IO()
main = do 
  -- Lazy I/O
  contents <- readFile "listOfPokemon.csv"
  
  -- Composing functions to generate the list of all pokemon objects from CSV file
  let allPokemon = (generateAllPokemon . parseComma . lines) contents
  
  -- Menu
  putStrLn "Please choose a number from below: "
  putStrLn "1) Print all Pokemon"
  putStrLn "2) Print all Pokemon types"
  putStrLn "3) Filter by type"
  putStrLn "4) Filter by index"
  putStrLn "5) Filter by name"
  
  optionSelected <- getChar
  flushLine <- getLine

  -- Perform logic based on user's decision from menu
  case optionSelected of
      '1' -> printEachPokemon allPokemon

      '2' -> printEachPokemonType

      '3' -> do 
          putStrLn "Please enter a Pokemon Type : "
          getTypeLine <- getLine
          let t = findPokeType getTypeLine
          printEachPokemon $ filterByType allPokemon t

      '4' -> do
          putStrLn "Please enter a Pokemon Index : "
          getIndexLine <- getLine
          let l = filterByIndex allPokemon (decimalStringToInt getIndexLine)
          case l of
              [] -> putStrLn "No Pokemon found!"
              _ -> printEachPokemon l

      '5' -> do
          putStrLn "Please enter a Pokemon Name : "
          getNameLine <- getLine
          let l = filterByName allPokemon getNameLine
          case l of
              [] -> putStrLn "No Pokemon found!"
              _ -> printEachPokemon l

      _   -> error "Invalid menu option! Please choose a number from the menu."
  

  putStrLn "Program Finished"

---------------------------- Calculate Effectiveness -----------------------
--
-- Function to calculate the effectiveness of a specific Pokemon against another defending Pokemon
-- based on it's type.
--
-- Example Usage: calculateEffectiveness (getType squirtle) (getType bulbasaur)
calculateEffectiveness :: PokeType -> PokeType -> Effectiveness

-- TODO: Add Fairy Type
calculateEffectiveness Fairy _ = error "TODO: Add Fairy Type!"

-- Normal Type
calculateEffectiveness Normal Rock = NotVeryEffective
calculateEffectiveness Normal Ghost = NoEffect 
calculateEffectiveness Normal _ = NormalEffectiveness

-- Fire Type
calculateEffectiveness Fire Fire = NotVeryEffective
calculateEffectiveness Fire Water = NotVeryEffective
calculateEffectiveness Fire Grass = SuperEffective
calculateEffectiveness Fire Ice = SuperEffective
calculateEffectiveness Fire Bug = SuperEffective
calculateEffectiveness Fire Rock = NotVeryEffective
calculateEffectiveness Fire Dragon = NotVeryEffective
calculateEffectiveness Fire _ = NormalEffectiveness

-- Water Type
calculateEffectiveness Water Fire = SuperEffective
calculateEffectiveness Water Water = NotVeryEffective
calculateEffectiveness Water Grass = NotVeryEffective
calculateEffectiveness Water Ground = SuperEffective
calculateEffectiveness Water Rock = SuperEffective
calculateEffectiveness Water Dragon = NotVeryEffective
calculateEffectiveness Water _ = NormalEffectiveness

-- Electric Type
calculateEffectiveness Electric Water = SuperEffective
calculateEffectiveness Electric Electric = NotVeryEffective 
calculateEffectiveness Electric Grass = NotVeryEffective
calculateEffectiveness Electric Ground = NoEffect
calculateEffectiveness Electric Flying = SuperEffective
calculateEffectiveness Electric Dragon = NotVeryEffective
calculateEffectiveness Electric _ = NormalEffectiveness

-- Grass Type
calculateEffectiveness Grass Fire = NotVeryEffective
calculateEffectiveness Grass Water = SuperEffective
calculateEffectiveness Grass Grass = NotVeryEffective
calculateEffectiveness Grass Poison = NotVeryEffective
calculateEffectiveness Grass Ground = SuperEffective
calculateEffectiveness Grass Flying = NotVeryEffective
calculateEffectiveness Grass Bug = NotVeryEffective
calculateEffectiveness Grass Rock = SuperEffective
calculateEffectiveness Grass Dragon = NotVeryEffective
calculateEffectiveness Grass _ = NormalEffectiveness

-- Ice Type
calculateEffectiveness Ice Water = NotVeryEffective
calculateEffectiveness Ice Grass = SuperEffective
calculateEffectiveness Ice Ice = NotVeryEffective
calculateEffectiveness Ice Ground = SuperEffective
calculateEffectiveness Ice Flying = SuperEffective
calculateEffectiveness Ice Dragon = SuperEffective
calculateEffectiveness Ice _ = NormalEffectiveness

-- Fighting Type
calculateEffectiveness Fighting Normal = SuperEffective
calculateEffectiveness Fighting Ice = SuperEffective
calculateEffectiveness Fighting Poison = NotVeryEffective
calculateEffectiveness Fighting Flying = NotVeryEffective
calculateEffectiveness Fighting Psychic = NotVeryEffective
calculateEffectiveness Fighting Bug = NotVeryEffective
calculateEffectiveness Fighting Rock = SuperEffective
calculateEffectiveness Fighting Ghost = NoEffect
calculateEffectiveness Fighting _ = NormalEffectiveness

-- Poison Type
calculateEffectiveness Poison Grass = SuperEffective
calculateEffectiveness Poison Poison = NotVeryEffective
calculateEffectiveness Poison Ground = NotVeryEffective
calculateEffectiveness Poison Bug = SuperEffective
calculateEffectiveness Poison Rock = NotVeryEffective
calculateEffectiveness Poison Ghost = NotVeryEffective
calculateEffectiveness Poison _ = NormalEffectiveness

-- Ground Type
calculateEffectiveness Ground Fire = SuperEffective
calculateEffectiveness Ground Electric = SuperEffective
calculateEffectiveness Ground Grass = NotVeryEffective
calculateEffectiveness Ground Poison = SuperEffective
calculateEffectiveness Ground Flying = NoEffect
calculateEffectiveness Ground Bug = NotVeryEffective
calculateEffectiveness Ground Rock = SuperEffective
calculateEffectiveness Ground _ = NormalEffectiveness

-- Flying Type
calculateEffectiveness Flying Electric = NotVeryEffective
calculateEffectiveness Flying Grass = SuperEffective
calculateEffectiveness Flying Fighting = SuperEffective
calculateEffectiveness Flying Bug = SuperEffective
calculateEffectiveness Flying Rock = NotVeryEffective
calculateEffectiveness Flying _ = NormalEffectiveness

-- Psychic Type
calculateEffectiveness Psychic Fighting = SuperEffective
calculateEffectiveness Psychic Poison = SuperEffective
calculateEffectiveness Psychic Psychic = NotVeryEffective
calculateEffectiveness Psychic _ = NormalEffectiveness

-- Bug Type
calculateEffectiveness Bug Fire = NotVeryEffective
calculateEffectiveness Bug Grass = SuperEffective
calculateEffectiveness Bug Fighting = NotVeryEffective
calculateEffectiveness Bug Poison = SuperEffective
calculateEffectiveness Bug Flying = NotVeryEffective
calculateEffectiveness Bug Ghost = NotVeryEffective
calculateEffectiveness Bug _ = NormalEffectiveness

-- Rock Type
calculateEffectiveness Rock Fire = SuperEffective
calculateEffectiveness Rock Ice = SuperEffective
calculateEffectiveness Rock Fighting = NotVeryEffective
calculateEffectiveness Rock Ground = NotVeryEffective
calculateEffectiveness Rock Flying = SuperEffective
calculateEffectiveness Rock Bug = SuperEffective
calculateEffectiveness Rock _ = NormalEffectiveness

-- Ghost Type
calculateEffectiveness Ghost Normal = NoEffect
calculateEffectiveness Ghost Psychic = NoEffect 
calculateEffectiveness Ghost Ghost = SuperEffective
calculateEffectiveness Ghost _ = NormalEffectiveness

-- Dragon Type
calculateEffectiveness Dragon Dragon = SuperEffective
calculateEffectiveness Dragon _ = NormalEffectiveness