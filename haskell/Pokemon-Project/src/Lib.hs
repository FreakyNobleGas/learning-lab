-- Name: Nicholas Quinn
--
-- Description: This file contains classes and functions that allow for the
-- creation of a unique Pokemon.
--
----------------------------------------------------------------------------

module Lib
    ( 
    testing,
    findPokeType,
    generatePokemon,
    generateAllPokemon,
    parseComma,
    calculateEffectiveness
    ) where

import System.IO
import Data.List (lines)
import Data.List.Split
import Text.XML.HXT.DOM.Util

--
-- This class allows each unique Pokemon to access it's various traits
-- and/or set them
--
class UniquePokemon p where
    getType :: p -> PokeType
    getName :: p -> String
    getIndex :: p -> Int 

--
-- Definition of methods defined in UniquePokemon class that retrieve information
-- about a specific Pokemon
--
instance UniquePokemon Pokemon where
    getType p = pokeType p
    getName p = name p
    getIndex p = index p

--
-- Pokemon Data Type
--
data Pokemon = Pokemon {
    name :: String,
    index :: Int,
    pokeType :: PokeType
} deriving (Show)

type PokemonDict = [Pokemon]

test :: PokemonDict
test = [bulbasaur, charmander]

--
-- Example of a Pokemon Data Type declarations.
--
bulbasaur = Pokemon {
    name = "Bulbasaur",
    index = 1,
    pokeType = Grass
}

charmander = Pokemon {
    name = "Charmander",
    index = 4,
    pokeType = Fire
}

squirtle = Pokemon {
    name = "Squirtle",
    index = 7,
    pokeType = Water
}

testing :: [(String, Int, PokeType)]
testing = [("test", 2, Grass)]

--findPokeType :: String -> PokeType
--findPokeType p = 

--generatePokemon :: [String] -> Pokemon
--generatePokemon (x:y:z) = Pokemon {x , y , z} 
generatePokemon :: [String] -> (String, Int, String)
generatePokemon p = 
    let name = p !! 0
        index = p !! 1 
        pokeType = p !! 2
    in (name, (decimalStringToInt index), pokeType)

--generateAllPokemon :: [[String]] -> PokemonDict
--generateAllPokemon p = let a = PokemonDict 
--                       in map generatePokemon p
generateAllPokemon :: [[String]] -> [(String, Int, String)]
generateAllPokemon p = map generatePokemon p

--
-- Function that opens a file stream to read in data from "ListOfPokemon.csv" into
-- an array with each Pokemon's characteristics seperated by a comma.
--
parseComma :: [String] -> [[String]]
parseComma p = map (splitOn ",") p

--
-- All 1st Generation Pokemon Types
--
data PokeType = Normal | Fire | Water | Electric | Grass | Ice |
                 Fighting | Poison | Ground | Flying | Psychic |
                 Bug | Rock | Ghost | Dragon | Dark | Steel
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

--
-- Function to calculate the effectiveness of a specific Pokemon against another defending Pokemon
-- based on it's type.
--
-- Example Usage: calculateEffectiveness (getType squirtle) (getType bulbasaur)
calculateEffectiveness :: PokeType -> PokeType -> Effectiveness

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