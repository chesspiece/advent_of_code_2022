module Day3 (day3) where

import Data.Char (ord, isAsciiLower)
import Data.Set (fromList, member, Set, intersection)
import Data.Maybe (fromMaybe)

parseString :: String -> [([Int], [Int])]
parseString = map (splitMiddle . map letter2priority) . lines

parseString2 :: String -> [[Int]]
parseString2 = map (map letter2priority) . lines

groupThree :: [a] -> [(a, a, a)]
groupThree (x:y:z:xs) = (x, y, z) : groupThree xs
groupThree _ = []

findCommon :: (Ord a, Num a) => ([a], [a], [a]) -> a
findCommon (aa, bb, cc) = fromMaybe 0 (valueInSet cc intersectedSet)
    where intersectedSet = intersection (fromList aa) (fromList bb) --Data.Set.filter (`member` set1) set2 
          --set1 = fromList aa
          --set2 = fromList bb

splitMiddle :: [a] -> ([a], [a])
splitMiddle inpt_str = splitAt (div (length inpt_str) 2) inpt_str

letter2priority:: Char -> Int
letter2priority c
    | isAsciiLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

valueInSet :: (Ord a, Num a) => [a] -> Set a -> Maybe a
valueInSet [] _ = Nothing
valueInSet (val:rest) st --st is a set with values of first half of rucksack (for first part of the puzzle)
    | member val st = Just val
    | otherwise = valueInSet rest st

checkInclusion :: (Ord a, Num a) => ([a], [a]) -> a
checkInclusion (a1, b1) = fromMaybe 0 (valueInSet b1 (fromList a1))
    -- case valueInSet b1 (fromList a1) of
    --    Nothing -> 0
    --    Just val -> val

day3 :: IO ()
day3 = do
    file_str <- readFile "task_3.txt"
    print $ sum . map checkInclusion . parseString $ file_str
    print $ sum . map findCommon . groupThree . parseString2 $ file_str
