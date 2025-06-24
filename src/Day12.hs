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

data MazeCoord = MazeCoord Int Int deriving (Show, Eq)

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
    [[MazeCoord]] ->
    -- return path
    Maybe MazeCoord
aStar startNode endNode maze = aStar' endNode (PQ.singleton (0, startNode))
  where
    aStar' ::
        -- end node
        MazeCoord ->
        -- priority queue of nodes
        PQ.MinQueue (Int, MazeCoord) ->
        -- return path
        Maybe MazeCoord
    aStar' nd2@(MazeCoord xEnd yEnd) pqNodes
        | PQ.null pqNodes = Nothing
        | currNode == nd2 = Just nd2
        | currNode /= nd2 =
            let
                (cost, currNode) = PQ.findMin pqNodes
                neighbours = undefined
            in undefined
        where
            (cost, currNode) = PQ.findMin pqNodes
            heuristic (MazeCoord x1 y1) = abs (xEnd -x1) + abs (yEnd - y1)
            neibours :: MazeCoord -> [MazeCoord]
            neibours = undefined

day12 :: IO ()
day12 = do
    txt <- readFile "task_12.txt"
    (indexes, maze) <- return . parse $ txt
    indexes <- return . fromJust $ indexes
    print maze
    print indexes