-- Name: Nicholas Quinn
--
-- Description: This file contains the logic to solve one of IGN's 2019 Code Foo
--              challenges. Please see readme.txt for the problem description.
--
----------------------------------------------------------------------------

import System.IO
import Data.List (lines, sortBy)
import Data.List.Split
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Graph
import Text.XML.HXT.DOM.Util

---------------------------- Classes and Data Types ------------------------
--
-- Class to represent data stored in Items
--
class ItemClass i where
    getType  :: i -> String
    getName  :: i -> String
    getCost  :: i -> Int
    getValue :: i -> Int
    getExtra :: i -> Bool

instance ItemClass Item where
    getType  i = armorType i
    getName  i = name i
    getCost  i = cost i
    getValue i = armorValue i
    getExtra i = extra i

--
-- Data type that holds information about each item
--
data Item = Item {
    armorType  :: String,
    name       :: String,
    cost       :: Int,
    armorValue :: Int,
    extra      :: Bool
} deriving (Show)

--
-- Type synonym's for each type of armor to make code more readable
--
{-
type Everything = [Item]
type Helmet     = [Item]
type Chest      = [Item]
type Leggings   = [Item]
type Boots      = [Item]

data Armory = Armory {
    everything :: [Item],
    helmet     :: [Item],
    chest      :: [Item],
    leggings   :: [Item],    
    boots      :: [Item]
} deriving (Show)

class ArmoryClass i where
    getEverything :: i -> [Item]

instance ArmoryClass Armory where
    getEverything i = everything i
-}

---------------------------- Helper Functions ------------------------------
--
-- Splits each comma seperated value into it's own [String]. This means every [String]  
-- represents a specific item in the armory
--
parseComma :: [String] -> [[String]]
parseComma p = map (splitOn ",") p

--
-- Generate Item from String value
--
generateItem :: [String] -> Item
generateItem i = 
    let newArmorType   = i !! 0
        newName        = i !! 1
        newCost        = decimalStringToInt $ i !! 2
        newArmorValue  = decimalStringToInt $ i !! 3
        newExtra       = False
    in Item {armorType = newArmorType, name = newName, cost = newCost, armorValue = newArmorValue, extra = newExtra}

--
-- Change Armor's boolean value to be an extra item
--
changeToExtra :: [Item] -> [Item]
changeToExtra allItems = map changeExtraBool allItems
                            where changeExtraBool i = Item {
                                armorType = getType i,
                                name = getName i,
                                cost = getCost i,
                                armorValue = getValue i,
                                extra = True
                            }

--
-- Generate all items from contents in CSV file
--
generateItems :: [[String]] -> [Item]
generateItems all = map generateItem all

--
-- Print list of items
--
printAllItems :: [Item] -> IO()
printAllItems i = mapM_ print i

--
-- Function to compare two pieces of armor by armor value, then cost
--
compareArmorValue x y
    | (getValue x) > (getValue y) = GT
    | (getValue x) < (getValue y) = LT
    | (getValue x) == (getValue y) = compare (getCost x) (getCost y)

--
-- Sort list of armor by it's value and then cost if they are equal
--
sortByValue :: [Item] -> [Item]
sortByValue newList = sortBy compareArmorValue newList

--
-- Sort list by armor type
--
sortByType :: [Item] -> String -> [Item]
sortByType allItems armorType = 
    let newList = filter isType allItems
    in (reverse . sortByValue) newList
        where isType i = armorType == (getType i)

--
-- Calculate the cost of a complete item set
--
calculateCost :: Item -> Item -> Item -> Item -> Item -> Int
calculateCost helmet chest legging boot extraItem =
    (getCost helmet) + (getCost chest) + (getCost legging) + (getCost boot) + (getCost extraItem)

--
-- Find the index of a specific piece of armor
--
itemLookUp :: String -> [Item] -> [(String, Int)] -> Item
itemLookUp armorType items selectedItems = items !! fromMaybe 0 (lookup armorType selectedItems)

--
-- Update the current index of a specific armor type
--
updateTuple :: String -> [Item] -> Bool -> [(String, Int)] -> (String, Int)
updateTuple armorType items increment selectedItems = 
    if increment && (((fromMaybe 0 (lookup armorType selectedItems)) + 1) < length items)
    then do (armorType,  (fromMaybe 0 (lookup armorType selectedItems)) + 1)
    else do (armorType,  fromMaybe 0  (lookup armorType selectedItems))

--
-- Based on the current indexes for each piece of armor, generate a complete item set
--
generateListOfItems :: [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item]
generateListOfItems selectedItems helmets chests leggings boots extraItems = [itemLookUp "helmet"    helmets    selectedItems,
                                                                             itemLookUp  "chest"     chests     selectedItems,
                                                                             itemLookUp  "leggings"  leggings   selectedItems,
                                                                             itemLookUp  "boots"     boots      selectedItems,
                                                                             itemLookUp  "extraItem" extraItems selectedItems]

--
-- Find each piece of armor that is next in list for each armor type, and return the one with the best armor value.
-- If two pieces of armor have the same value, then return the one that costs the most.
--
calculateBestItem :: [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> Item
calculateBestItem selectedItems helmets chests leggings boots extraItems = 
    let newList = [ updateTuple "helmet" helmets True selectedItems,
                    updateTuple "chest" chests True selectedItems,
                    updateTuple "leggings" leggings True selectedItems,
                    updateTuple "boots" boots True selectedItems,
                    updateTuple "extraItem" extraItems True selectedItems
                  ]
    in compareValues (generateListOfItems newList helmets chests leggings boots extraItems)
                     (generateListOfItems selectedItems helmets chests leggings boots extraItems)
                     0
                     500
                     0
                 

compareValues :: [Item] -> [Item] -> Int -> Int -> Int -> Item
compareValues newList oldList index diffValue bestItem = 
    if index > 4
    then (newList !! bestItem)
    else
        let newDiffValue = (getValue (oldList !! index) - getValue(newList !! index)) 
        in if (newDiffValue < diffValue) &&  (newDiffValue /= 0)
           then compareValues newList oldList (index + 1) newDiffValue index
           else
               compareValues newList oldList (index + 1) diffValue bestItem
        

--
-- After finding the best item, update the current selected item for the correct armor type
--
updateSelectedItems :: [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> [(String, Int)]
updateSelectedItems selectedItems helmets chests leggings boots extraItems = 
    let newItem = calculateBestItem selectedItems helmets chests leggings boots extraItems
    in if (getExtra newItem)
       then  
           [ updateTuple "helmet" helmets False selectedItems,
             updateTuple "chest" chests False selectedItems,
             updateTuple "leggings" leggings False selectedItems,
             updateTuple "boots" boots False selectedItems,
             updateTuple "extraItem" extraItems True selectedItems
            ] 
        else case (getType newItem) of
            "Helmet" -> [
                         updateTuple "helmet" helmets True selectedItems,
                         updateTuple "chest" chests False selectedItems,
                         updateTuple "leggings" leggings False selectedItems,
                         updateTuple "boots" boots False selectedItems,
                         updateTuple "extraItem" extraItems False selectedItems
                        ]
            "Chest" -> [
                         updateTuple "helmet" helmets False selectedItems,
                         updateTuple "chest" chests True selectedItems,
                         updateTuple "leggings" leggings False selectedItems,
                         updateTuple "boots" boots False selectedItems,
                         updateTuple "extraItem" extraItems False selectedItems
                        ]
            "Leggings" -> [
                         updateTuple "helmet" helmets False selectedItems,
                         updateTuple "chest" chests False selectedItems,
                         updateTuple "leggings" leggings True selectedItems,
                         updateTuple "boots" boots False selectedItems,
                         updateTuple "extraItem" extraItems False selectedItems
                        ]
            "Boots" -> [
                         updateTuple "helmet" helmets False selectedItems,
                         updateTuple "chest" chests False selectedItems,
                         updateTuple "leggings" leggings False selectedItems,
                         updateTuple "boots" boots True selectedItems,
                         updateTuple "extraItem" extraItems False selectedItems
                        ]
            _ -> error "Could not find type!"

--
-- Calculate the cost of the current set of armor and if it costs more than 300 crowns, calculate the cost of the
-- next possible combination of armor until the total cost is less than 300 crowns.
--
calculateResult :: Int -> [(String, Int)] -> [Item] -> [Item] -> [Item] -> [Item] -> [Item] -> [(String, Int)]
calculateResult cost selectedItems helmets chests leggings boots extraItems = 
    if cost <= 300 
    then do selectedItems
    else let updatedItems = updateSelectedItems selectedItems helmets chests leggings boots extraItems
         in calculateResult (calculateCost (itemLookUp "helmet" helmets updatedItems)
                            (itemLookUp "chest" chests updatedItems)
                            (itemLookUp "leggings" leggings updatedItems)
                            (itemLookUp "boots" boots updatedItems)
                            (itemLookUp "extraItem" extraItems updatedItems))
                            updatedItems
                            helmets
                            chests
                            leggings
                            boots
                            extraItems
    
---------------------------- Main Driver -----------------------------------
--
-- Main Driver
--
main :: IO()
main = do
    contents <- readFile "witcher-inventory.csv"

    -- Convert all contents from file into an array of Items so that it can be used
    -- more efficiently
    let allItems = (reverse . sortByValue . generateItems . parseComma . tail . lines) contents
    
    -- Sort each list so that it is by best to worst armor value
    let helmets  = sortByType allItems "Helmet"
    let chests   = sortByType allItems "Chest"
    let leggings = sortByType allItems "Leggings"
    let boots    = sortByType allItems "Boots"
    let extraItems = changeToExtra allItems
    
    -- Dictionary for each armor type and it's current index
    let selectedItems = [("helmet", 0), ("chest", 0), ("leggings", 0), ("boots", 0), ("extraItem", 0)]

    -- Find the cost of the best possible armor set
    let cost = calculateCost (itemLookUp "helmet"    helmets    selectedItems)
                             (itemLookUp "chest"     chests     selectedItems)
                             (itemLookUp "leggings"  leggings   selectedItems)
                             (itemLookUp "boots"     boots      selectedItems)
                             (itemLookUp "extraItem" extraItems selectedItems)
    
    -- Find best possible combination of armor under 300 crowns
    let result = generateListOfItems (calculateResult cost
                                                     selectedItems
                                                     helmets
                                                     chests
                                                     leggings
                                                     boots
                                                     extraItems)
                                     helmets
                                     chests
                                     leggings
                                     boots
                                     extraItems

    putStrLn "---RESULT---"
    printAllItems result
   
    putStrLn "---COST---"
    print (calculateCost (result !! 0)
                         (result !! 1)
                         (result !! 2)
                         (result !! 3)
                         (result !! 4))

    putStrLn "Program Finished!"