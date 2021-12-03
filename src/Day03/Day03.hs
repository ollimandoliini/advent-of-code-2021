module Day03.Day03 where

import Data.Void
import Data.List (transpose)
import Data.Map
import qualified Control.Applicative as Map
import Data.List
import Data.Function

data Bit = One | Zero deriving (Show, Eq, Ord)

toBit :: Char -> Bit
toBit '0' = Zero
toBit '1' = One
toBit _ = undefined

toDecimal :: [Bit] -> Int
toDecimal = foldl' 

mostFrequentBit :: Ord c => [c] -> c
mostFrequentBit bits = fst . maximumBy (compare `on` snd) $ toList $ go empty bits
  where
    go acc [] = acc
    go acc (x:xs) = go (insertWith (+) x 1 acc) xs

part1 = do
    fileLines <- lines <$> readFile "src/Day03/input.txt"
    let bits = (fmap . fmap) toBit fileLines
    print $ mostFrequentBit <$> transpose bits

