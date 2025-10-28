{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Move guards forward" #-}

module Day12 (day12) where

import Control.Monad.Trans (lift)
import Data.Char (isAsciiLower, ord)
import Data.List (findIndex, findIndices, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Vector as V
import qualified Data.PSQueue as PSQ

data MazeCoord = MazeCoord Int Int deriving (Show, Eq, Ord)

data Maze = Maze (V.Vector (V.Vector Int)) Int Int deriving (Show, Eq)

type MazeBool = V.Vector (V.Vector Bool)
type MazeGscore = V.Vector (V.Vector (Maybe Int))

fromList :: [Int] -> MazeCoord
fromList [x, y] = MazeCoord x y
fromList _ = error "Should be impossible in this task"

getMap :: Maze -> V.Vector (V.Vector Int)
getMap (Maze map _ _) = map

getMaxCoord :: Maze -> (Int, Int)
getMaxCoord (Maze _ maxRow maxColumn) = (maxRow, maxColumn)

parse :: String -> (Maybe (MazeCoord, MazeCoord), Maze)
parse inputText =
    let
        inputMatrixStr = lines inputText
        startEndIndexes = findStartEnd inputMatrixStr 0 Nothing Nothing
        numRows = length inputMatrixStr
        numColumns = length $ head inputMatrixStr
    in
        ( startEndIndexes
        , Maze
            (V.fromList $ map (V.fromList . map letter2elevation) inputMatrixStr)
            numRows
            numColumns
        )

checkStart :: [Char] -> Maybe Int
checkStart = findIndex $ \s -> s == 'S'

checkEnd :: [Char] -> Maybe Int
checkEnd = findIndex $ \s -> s == 'E'

elevationAt :: Maze -> MazeCoord -> Int
elevationAt (Maze grid _ _) (MazeCoord r c) = (grid V.! r) V.! c

gScoreAt :: MazeGscore -> MazeCoord -> Maybe Int
gScoreAt grid (MazeCoord r c) = (grid V.! r) V.! c

visitedCheck :: MazeBool -> MazeCoord -> Bool
visitedCheck grid (MazeCoord r c) = (grid V.! r) V.! c

findStartEnd ::
    [String] ->
    Int ->
    Maybe MazeCoord ->
    Maybe MazeCoord ->
    Maybe (MazeCoord, MazeCoord)
findStartEnd [] rowIdx Nothing _ = error "Should be impossible in this task"
findStartEnd [] rowIdx _ Nothing = error "Should be impossible in this task"
findStartEnd text rowIdx (Just startIdx) (Just endIdx) = Just (startIdx, endIdx)
findStartEnd (row : nextRows) rowIdx Nothing Nothing =
    let
        startIdx = fromList <$> sequence [Just rowIdx, checkStart row]
        endIdx = fromList <$> sequence [Just rowIdx, checkEnd row]
    in
        findStartEnd nextRows (rowIdx + 1) startIdx endIdx
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
neighbours (MazeCoord row col) (Maze maze maxRows maxColumns) =
    let
        a =
            if (col + 1) < maxColumns
                then Just $ MazeCoord row (col + 1)
                else Nothing
        b =
            if (col - 1) >= 0
                then Just $ MazeCoord row (col - 1)
                else Nothing
        c =
            if (row + 1) < maxRows
                then Just $ MazeCoord (row + 1) col
                else Nothing
        d =
            if (row - 1) >= 0
                then Just $ MazeCoord (row - 1) col
                else Nothing
    in
        fromJust . sequence $ filter isJust [a, b, c, d]

neighborsClimbOK :: Maze -> MazeBool -> MazeCoord -> [MazeCoord]
neighborsClimbOK maze mazeBool coord =
    let currentElevation = elevationAt maze coord
    in  filter
            ( \v ->
                (elevationAt maze v <= currentElevation + 1) && not (visitedCheck mazeBool v)
            )
            (neighbours coord maze)

manhattanDistance ::
    -- current coordinate
    MazeCoord ->
    -- desired end coordinate
    MazeCoord ->
    Int
manhattanDistance currCoord@(MazeCoord x1 y1) (MazeCoord xEnd yEnd) = abs (xEnd - x1) + abs (yEnd - y1)

-- Update visited status in MazeBool. This looks bad, but it's how it's done in Vector
setVisited :: MazeBool -> MazeCoord -> MazeBool
setVisited mazeBool (MazeCoord r c) =
    mazeBool V.// [(r, (mazeBool V.! r) V.// [(c, True)])]

setGScore :: MazeGscore -> MazeCoord -> Int -> MazeGscore
setGScore grid (MazeCoord r c) score =
    grid V.// [(r, (grid V.! r) V.// [(c, Just score)])]

aStar ::
    -- start node
    MazeCoord ->
    -- end node
    MazeCoord ->
    -- Maze
    Maze ->
    -- return path length
    Maybe Int
aStar startNode endNode maze@(Maze _ maxRows maxColumns) =
    let
        initialMazeBool = V.replicate maxRows (V.replicate maxColumns False)
        -- Start with f-score = heuristic distance
        initialFScore = manhattanDistance startNode endNode
        initialGscore = V.replicate maxRows (V.replicate maxColumns Nothing)
    in
        aStar'
            (PSQ.singleton (0, startNode) initialFScore)
            initialMazeBool
            initialGscore
  where
    aStar' ::
        -- priority queue of nodes: (f-score, g-score, node)
        PSQ.PSQ (Int, MazeCoord) Int ->
        -- needed to check in O(1) if node was visited
        MazeBool ->
        -- g-scores (actual distances from start)
        MazeGscore ->
        -- return path length
        Maybe Int
    aStar' pqNodes mazeBool gScores
        | PSQ.null pqNodes = Nothing
        | currNode == endNode = Just currG
        -- probably need to use decrease key for repeated nodes. But I don't know how to do it in haskell right now
        | visitedCheck mazeBool currNode = aStar' pqNodesRest mazeBool gScores
        | otherwise =
            let
                -- Mark current node as visited
                newMazeBool = setVisited mazeBool currNode
                -- Update g-scores
                newGScores = setGScore gScores currNode currG
                -- Get valid neighbors
                validNeighbors = neighborsClimbOK maze newMazeBool currNode
                -- Calculate scores for each neighbor
                neighborScores =
                    [ (gScore, hScore, node) | node <- validNeighbors, let gScore = currG + 1, let hScore = manhattanDistance node endNode,
                    -- should do decrease key for nodes with gScore < gScores M.! n. But don't know how for now.
                    isNothing (gScoreAt gScores node) || gScore < fromJust (gScoreAt gScores node)
                    ]
                -- Add neighbors to priority queue with f-score = g-score + h-score
                newPQ =
                    foldl' (\pq (g, h, n) -> PSQ.insert (g, n) (g + h) pq) pqNodesRest neighborScores
            in
                aStar' newPQ newMazeBool newGScores
      where
        (currG, currNode)  = PSQ.key  . fromJust $ PSQ.findMin pqNodes
        pqNodesRest = PSQ.deleteMin pqNodes

day12 :: IO ()
day12 = do
    txt <- readFile "task_12.txt"
    (indexes, maze) <- return . parse $ txt
    indexes <- return . fromJust $ indexes
    let (start, end) = indexes
    print $ "Start: " ++ show start
    print $ "End: " ++ show end

    case aStar start end maze of
        Nothing -> putStrLn "No path found"
        Just pathLength -> putStrLn $ "Part 1: " ++ show pathLength