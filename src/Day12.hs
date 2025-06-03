{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

module Day12 (day12) where

import Control.Monad.Trans (lift)
import Data.Char (isAsciiLower, ord)
import Data.List (findIndex, findIndices)
import Data.Maybe (fromJust)

newtype MazeCoord = MazeCoord (Int, Int)

parse :: String -> (Maybe (Int, Int), [[Int]])
parse inputText =
    let inputMatrixStr = lines inputText
        indexes = findStartEnd inputMatrixStr 0 Nothing Nothing
    in  (indexes, map (map letter2elevation) inputMatrixStr)

checkStart :: [Char] -> Maybe Int
checkStart = findIndex $ \s -> s == 'S'

checkEnd :: [Char] -> Maybe Int
checkEnd = findIndex $ \s -> s == 'E'

findStartEnd :: [String] -> Int -> Maybe Int -> Maybe Int -> Maybe (Int, Int)
findStartEnd [] _ Nothing _ = Nothing
findStartEnd [] _ _ Nothing = Nothing
findStartEnd text _ (Just startIdx) (Just endIdx) = Just (startIdx, endIdx)
findStartEnd (row : nextRows) _ Nothing Nothing =
    let startIdx = checkStart row
        endIdx = checkEnd row
    in  findStartEnd nextRows 0 startIdx endIdx
findStartEnd (row : nextRows) _ (Just startIdx) Nothing =
    let endIdx = checkEnd row
    in  findStartEnd nextRows 0 (Just startIdx) endIdx
findStartEnd (row : nextRows) _ Nothing (Just endIdx) =
    let startIdx = checkStart row
    in  findStartEnd nextRows 0 startIdx (Just endIdx)

letter2elevation :: Char -> Int
letter2elevation 'S' = ord 'a' - ord 'a'
letter2elevation 'E' = ord 'z' - ord 'a'
letter2elevation c = ord c - ord 'a'

day12 :: IO ()
day12 = do
    txt <- readFile "task_12.txt"
    (indexes, maze) <- return . parse $ txt
    indexes <- return . fromJust $ indexes
    print maze
    print indexes
