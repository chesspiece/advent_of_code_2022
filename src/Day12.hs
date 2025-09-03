{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

module Day12 (day12) where

import Control.Monad.Trans (lift)
import Data.Char (isAsciiLower, ord)
import Data.List (findIndex, findIndices)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import qualified Data.PQueue.Min as PQ
import qualified Data.Vector as V

data MazeCoord = MazeCoord Int Int deriving (Show, Eq)

data Maze = Maze (V.Vector (V.Vector Int)) Int Int deriving (Show, Eq)

fromList :: [Int] -> MazeCoord
fromList [x, y] = MazeCoord x y
fromList _ = error "Should be impossible in this task"

getMap :: Maze -> V.Vector (V.Vector Int)
getMap (Maze map _ _) = map

getMaxCoord :: Maze -> (Int, Int)
getMaxCoord (Maze _ maxRow maxColumn) = (maxRow, maxColumn)

parse :: String -> (Maybe (MazeCoord, MazeCoord), Maze)
parse inputText =
    let inputMatrixStr = lines inputText
        startEndIndexes = findStartEnd inputMatrixStr 0 Nothing Nothing
        numRows = length inputMatrixStr
        numColumns = length $ head inputMatrixStr
    in  ( startEndIndexes
        , Maze (V.fromList $ map (V.fromList . map letter2elevation) inputMatrixStr) numRows numColumns
        )

checkStart :: [Char] -> Maybe Int
checkStart = findIndex $ \s -> s == 'S'

checkEnd :: [Char] -> Maybe Int
checkEnd = findIndex $ \s -> s == 'E'

elevationAt :: Maze -> MazeCoord -> Int
elevationAt (Maze grid _ _) (MazeCoord r c) = (grid V.! r) V.! c

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

neighbours :: MazeCoord -> Maze -> [MazeCoord]
neighbours (MazeCoord x y) (Maze maze maxRows maxColumns) =
    let
        a =
            if (x + 1) < maxColumns
                then Just $ MazeCoord (x + 1) y
                else Nothing
        b =
            if (x - 1) >= 0
                then Just $ MazeCoord (x - 1) y
                else Nothing
        c =
            if (y + 1) < maxRows
                then Just $ MazeCoord x (y + 1)
                else Nothing
        d =
            if (y - 1) >= 0
                then Just $ MazeCoord x (y - 1)
                else Nothing
    in
        fromJust . sequence $ filter isJust [a, b, c, d]

neighborsClimbOK :: Maze -> MazeCoord -> [MazeCoord]
neighborsClimbOK maze coord =
    let currentElevation = elevationAt maze coord
    in  filter (\v -> elevationAt maze v <= currentElevation + 1) (neighbours coord maze)

manhattanDistance ::
    -- current coordinate
    MazeCoord ->
    -- desired end coordinate
    MazeCoord ->
    Int
manhattanDistance (MazeCoord x1 y1) (MazeCoord xEnd yEnd) = abs (xEnd - x1) + abs (yEnd - y1)

aStar ::
    -- start node
    MazeCoord ->
    -- end node
    MazeCoord ->
    -- Maze
    Maze ->
    -- return path
    Maybe MazeCoord
aStar startNode endNode maze = aStar' endNode (PQ.singleton (0, startNode)) maze
  where
    aStar' ::
        -- end node
        MazeCoord ->
        -- priority queue of nodes
        PQ.MinQueue (Int, MazeCoord) ->
        Maze ->
        -- return path
        Maybe MazeCoord
    aStar' nd2@(MazeCoord xEnd yEnd) pqNodes maze
        | PQ.null pqNodes = Nothing
        | currNode == nd2 = Just nd2
        | currNode /= nd2 =
            let neighbours = undefined
            in  undefined
      where
        (cost, currNode) = PQ.findMin pqNodes

-- currG = gScore M.! curr
-- Manheatten distance

day12 :: IO ()
day12 = do
    txt <- readFile "task_12.txt"
    (indexes, maze) <- return . parse $ txt
    indexes <- return . fromJust $ indexes
    print maze
    let mp = getMap maze
    let (maxR, maxC) = getMaxCoord maze
    print $ (mp V.! (maxR - 1)) V.! (maxC - 1)
    print indexes