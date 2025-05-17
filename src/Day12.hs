{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

module Day12 (day12) where

import Data.Char (isAsciiLower, ord)
import Data.List (findIndex, findIndices)
import Data.Vector.Generic (streamR)

parseString :: String -> ([Maybe Int], [Maybe Int])
parseString input_text =
    let
        start_idx = map checkStart $ lines input_text
        end_idx = map checkStart $ lines input_text
     in
        (start_idx, end_idx)

checkStart :: [Char] -> Maybe Int
checkStart = findIndex $ \s -> s == 'S'

checkEnd :: [Char] -> Maybe Int
checkEnd = findIndex $ \s -> s == 'E'

findStartEnd :: [String] -> Maybe Int -> Maybe Int -> Maybe (Int, Int)
findStartEnd [] Nothing _ = Nothing
findStartEnd [] _ Nothing = Nothing
findStartEnd text (Just startIdx) (Just endIdx) = Just (startIdx, endIdx)
findStartEnd (row : nextRows) Nothing Nothing =
    let
        startIdx = checkStart row
        endIdx = checkEnd row
     in
        findStartEnd nextRows startIdx endIdx
findStartEnd (row : nextRows) (Just startIdx) Nothing =
    let
        endIdx = checkEnd row
     in
        findStartEnd nextRows (Just startIdx) endIdx
findStartEnd (row : nextRows) Nothing (Just endIdx) =
    let
        startIdx = checkStart row
     in
        findStartEnd nextRows startIdx (Just endIdx)

letter2elevation :: Char -> Int
letter2elevation 'S' = ord 'a' - ord 'a'
letter2elevation 'E' = ord 'z' - ord 'a'
letter2elevation c = ord c - ord 'a'

day12 :: IO ()
day12 = do
    txt <- readFile "task_12.txt"
    print "yay!"