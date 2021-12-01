module Day01.Day01 where

import Text.Read ( readMaybe )
import Data.Maybe ( mapMaybe )



parseFile :: String -> [Int]
parseFile s = do
    mapMaybe readMaybe (lines s)

part1 = do
    numbers <- parseFile <$> readFile "src/Day01/input.txt"
    print $ length $ filter (uncurry (<)) $ zip numbers (tail numbers)

part2 = do
    numbers <- parseFile <$> readFile "src/Day01/input.txt"
    let windowSums = windowSum <$> windows numbers
    print $ length $ filter (uncurry (<)) $ zip windowSums (tail windowSums)
    where
        windows :: [Int] -> [(Int, Int, Int)] 
        windows numbers = zip3 numbers (tail numbers) ((tail . tail) numbers)
        windowSum :: (Int, Int, Int) -> Int
        windowSum (a, b, c) = a + b + c


