-- Name: Nicholas Quinn
--
-- Description: Case study on Barcode Recognition
--

{-
    This case study focuses on the EAN-13 barcode standard, which is a 13-digit sequence broken
    into 4 groups. The barcodes will be captured using a netpbm file format using low end fixed 
    resolution cameras.

    First 2 digits describe the number system, such as the nationality of the manufactor and other
    characteristics.

    Next 5 digits is the manufactor ID

    The next 5 digits is the product ID

    The last digit is the check digit, which allows a scanner to validate the digit string
    it scanned.

    A barcode is a series of fixed width bars, where a black bar represents a 1 and white is 0.
-}

import Data.Array (Array(..), (!), bounds, elems, indices,
                   ixmap, listArray)

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

-- Imported from Chapter 10
import ParseLib  

-- Locates the last digit of the barcode to determine it's check digit. The check digit is calculated
-- by taking the difference between the sum of every 3rd number (multiplied by 3 off and on) mod 10 and then
-- subtract 10
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
    where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])

-- Haskell's list data type must be accessed linearly until the k'th element. Tuples do not have this problem,
-- but is difficult to write a function that returns an element at a specific offset.
--
-- For this case study, using an Array type would be the best solution in terms of efficiency, however they are immutable.
-- If we decide to change on element, we would have to copy the entire array which is expensive. (Note: There is something called
-- a ST monad that does make it efficient to change an element in an array)

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

-- ListArray populates an array based on a list
listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,l-1) xs
    where l = length xs

-- The Array type is paramerterised over both the data it contains and the index type
-- Example of creating array: array (0,2) [(0,'f'),(1,'o'),(2,'o')]
-- Arrays can be index by the '!' operator. Example: a ! 2
leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

-- | Strict left fold, similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
-- The indices function returns a list of every index
foldA f s a = go s (indices a)
    where go s (j:js) = let s' = f s (a ! j)
                        in s' `seq` go s' js
          go s _ = s

-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
-- The bounds function returns a tuple describing the bounds that are used to create the array
foldA1 f a = foldA f (a ! fst (bounds a)) a

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

-- | This function computes the check digit; don't pass one in.
encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard = "101"
centerGuard = "01010"


-- For decoding an image, we want to seperate white and black pixels for the lines of the barcode.
type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)

type Pixmap = Array (Int,Int) RGB

parseRawPPM :: Parse Pixmap
parseRawPPM =
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P6") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxValue ->
    assert (maxValue == 255) "max value out of spec" ==>&
    parseByte ==>&
    parseTimes (width * height) parseRGB ==> \pxs ->
    identity (listArray ((0,0),(width-1,height-1)) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte ==> \r ->
           parseByte ==> \g ->
           parseByte ==> \b ->
           identity (r,g,b)

-- Calls a parser repeatingly, builds up results
parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p ==> \x -> (x:) <$> parseTimes (n-1) p

-- Convert Colour data into Monochrome

luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r,g,b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
    where r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b

type Greymap = Array (Int,Int) Pixel

-- Only for illustration
pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance

-- Greyscale to Binary
-- We need to convert the greyscale image into a two-valued image, where each pixel
-- is either on or off

data Bit = Zero | One
           deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
    where binary i | i < pivot  = Zero
                   | otherwise  = One
          pivot    = round $ least + (greatest - least) * n
          least    = fromIntegral $ choose (<) a
          greatest = fromIntegral $ choose (>) a
          choose f = foldA1 $ \x y -> if f x y then x else y

-- Tackles problem of bar thickness
type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group
    where rle xs = (length xs, head xs)

runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
    where divide d = fromIntegral d / divisor
          divisor = fromIntegral (sum xs)
-- A more compact alternative that "knows" we're using Ratio Int:
-- scaleToOne xs = map (% sum xs) xs

type ScoreTable = [[Score]]

-- "SRL" means "scaled run length".
asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

bestScores :: ScoreTable -> [Run] -> [(Score, Word8)]
bestScores srl ps = take 3 . sort $ scores
    where scores = zip [distance d (scaleToOne ps) | d <- srl] digits
          digits = [0..9]

data Parity a = Even a | Odd a | None a
                deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
    fmap = parityMap

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g x y = g x `f` g y

compareWithoutParity = compare `on` fromParity

type Digit = Word8

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
              ((map Odd (bestScores leftOddSRL ps)) ++
               (map Even (bestScores leftEvenSRL ps)))

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

data AltParity a = AltEven {fromAltParity :: a}
                 | AltOdd  {fromAltParity :: a}
                 | AltNone {fromAltParity :: a}
                   deriving (Show)

chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs
                 in h : chunkWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One):_) = []
candidateDigits rle | length rle < 59 = []

candidateDigits rle
    | any null match = []
    | otherwise      = map (map (fmap snd)) match
  where match = map bestLeft left ++ map bestRight right
        left = chunksOf 4 . take 24 . drop 3 $ runLengths
        right = chunksOf 4 . take 24 . drop 32 $ runLengths
        runLengths = map fst rle

type Map a = M.Map Digit [a]

type DigitMap = Map Digit
type ParityMap = Map (Parity Digit)

updateMap :: Parity Digit       -- ^ new digit
          -> Digit              -- ^ existing key
          -> [Parity Digit]     -- ^ existing digit sequence
          -> ParityMap          -- ^ map to update
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit =
    new `M.union` M.foldrWithKey (updateMap digit) M.empty old

incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old digits = foldl' (useDigit old) M.empty digits

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 [])
            . mapEveryOther (map (fmap (*3)))

firstDigit :: [Parity a] -> Digit
firstDigit = snd
           . head
           . bestScores paritySRL
           . runLengths
           . map parityBit
           . take 6
  where parityBit (Even _) = Zero
        parityBit (Odd _) = One

addFirstDigit :: ParityMap -> DigitMap
addFirstDigit = M.foldrWithKey updateFirst M.empty

updateFirst :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
updateFirst key seq = insertMap key digit (digit:renormalize qes)
  where renormalize = mapEveryOther (`div` 3) . map fromParity
        digit = firstDigit qes
        qes = reverse seq

buildMap :: [[Parity Digit]] -> DigitMap
buildMap = M.mapKeys (10 -)
         . addFirstDigit
         . finalDigits

solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where checkDigits = map fromParity (last xs)
          m = buildMap (init xs)
          addCheckDigit m k = (++[k]) <$> M.lookup k m

withRow :: Int -> Pixmap -> (RunLength Bit -> a) -> a
withRow n greymap f = f . runLength . elems $ posterized
    where posterized = threshold 0.4 . fmap luminance . row n $ greymap

row :: (Ix a, Ix b) => b -> Array (a,b) c -> Array a c
row j a = ixmap (l,u) project a
    where project i = (i,j)
          ((l,_), (u,_)) = bounds a

findMatch :: [(Run, Bit)] -> Maybe [[Digit]]
findMatch = listToMaybe
          . filter (not . null)
          . map (solve . candidateDigits)
          . tails

findEAN13 :: Pixmap -> Maybe [Digit]
findEAN13 pixmap = withRow center pixmap (fmap head . findMatch)
  where (_, (maxX, _)) = bounds pixmap
        center = (maxX + 1) `div` 2

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> do
    e <- parse parseRawPPM <$> L.readFile arg
    case e of
      Left err ->     print $ "error: " ++ err
      Right pixmap -> print $ findEAN13 pixmap