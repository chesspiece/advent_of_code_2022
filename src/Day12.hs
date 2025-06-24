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

-- import Data.Set

import qualified Data.PQueue.Min as PQ
--import Data.Vector as V

data MazeCoord = MazeCoord Int Int deriving (Show)

fromList :: [Int] -> MazeCoord
fromList [x, y] = MazeCoord x y
fromList _ = error "Should be impossible in this task"

parse :: String -> (Maybe (MazeCoord, MazeCoord), [[Int]])
parse inputText =
    let inputMatrixStr = lines inputText
        indexes = findStartEnd inputMatrixStr 0 Nothing Nothing
    in  (indexes, map (map letter2elevation) inputMatrixStr)

checkStart :: [Char] -> Maybe Int
checkStart = findIndex $ \s -> s == 'S'

checkEnd :: [Char] -> Maybe Int
checkEnd = findIndex $ \s -> s == 'E'

findStartEnd ::
    [String] -> Int -> Maybe MazeCoord -> Maybe MazeCoord -> Maybe (MazeCoord, MazeCoord)
findStartEnd [] rowIdx Nothing _ = error "Should be impossible in this task"
findStartEnd [] rowIdx _ Nothing = error "Should be impossible in this task"
findStartEnd text rowIdx (Just startIdx) (Just endIdx) = Just (startIdx, endIdx)
findStartEnd (row : nextRows) rowIdx Nothing Nothing =
    let startIdx = fromList <$> sequence [Just rowIdx, checkStart row]
        endIdx = fromList <$> sequence [Just rowIdx, checkEnd row]
    in  findStartEnd nextRows (rowIdx + 1) startIdx endIdx
findStartEnd (row : nextRows) rowIdx (Just startIdx) Nothing =
    let endIdx = fromList <$> sequence [Just rowIdx, checkEnd row]
    in  findStartEnd nextRows (rowIdx + 1) (Just startIdx) endIdx
findStartEnd (row : nextRows) rowIdx Nothing (Just endIdx) =
    let startIdx = fromList <$> sequence [Just rowIdx, checkStart row]
    in  findStartEnd nextRows (rowIdx + 1) startIdx (Just endIdx)

letter2elevation :: Char -> Int
letter2elevation 'S' = ord 'a' - ord 'a'
letter2elevation 'E' = ord 'z' - ord 'a'
letter2elevation c = ord c - ord 'a'

aStar ::
    -- start node
    MazeCoord ->
    -- end node
    MazeCoord ->
    -- Maze
    [[MazeCoord]]
aStar = undefined
  where
    aStar' ::
        -- start  Node
        MazeCoord ->
        -- end node
        MazeCoord ->
        -- cost func
        (MazeCoord -> Int) ->
        -- heuristic func
        (MazeCoord -> Int) ->
        -- closed set of visited nodes
        a ->
        -- priority queue of nodes
        b
    aStar' = undefined

day12 :: IO ()
day12 = do
    txt <- readFile "task_12.txt"
    (indexes, maze) <- return . parse $ txt
    indexes <- return . fromJust $ indexes
    print maze
    print indexes