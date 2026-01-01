{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Move guards forward" #-}

module Day12 (day12) where

import Control.Monad (filterM)
import Control.Monad.Trans (lift)
import Data.Char (isAsciiLower, ord)
import Data.List (findIndex, findIndices, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.PSQueue as PSQ
import qualified Data.Sequence as DS
import qualified Data.Vector as V

-- import Control.Parallel.Strategies (parMap, rdeepseq)

import Control.Exception.Lens (exception)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (MArray)
import Data.Array.ST (STArray, STUArray, newArray, readArray, writeArray)
import System.CPUTime (getCPUTime)

data MazeCoord = MazeCoord Int Int deriving (Show, Eq, Ord)

data Maze = Maze (V.Vector (V.Vector Int)) Int Int deriving (Show, Eq)

fromList :: [Int] -> MazeCoord
fromList [x, y] = MazeCoord x y
fromList _ = error "Should be impossible in this task"

getMap :: Maze -> V.Vector (V.Vector Int)
getMap (Maze map _ _) = map

getMaxCoord :: Maze -> (Int, Int)
getMaxCoord (Maze _ maxRow maxColumn) = (maxRow, maxColumn)

parse :: String -> (Maybe (MazeCoord, [MazeCoord], MazeCoord), Maze)
parse inputText =
    let
        inputMatrixStr = lines inputText
        startEndIndexes = findStartEnd inputMatrixStr 0 Nothing [] Nothing
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

gScoreAt :: STArray s (Int, Int) (Maybe Int) -> MazeCoord -> ST s (Maybe Int)
gScoreAt grid (MazeCoord r c) = readArray grid (r, c)

visitedCheck :: STUArray s (Int, Int) Bool -> MazeCoord -> ST s Bool
visitedCheck grid (MazeCoord r c) = readArray grid (r, c)

findStartEnd ::
    [String] ->
    Int ->
    Maybe MazeCoord ->
    [MazeCoord] ->
    Maybe MazeCoord ->
    Maybe (MazeCoord, [MazeCoord], MazeCoord)
findStartEnd [] _ (Just startIdx) possibleStarts (Just endIdx) = Just (startIdx, startIdx : possibleStarts, endIdx)
findStartEnd [] _ Nothing _ _ = error "Should be impossible in this task"
findStartEnd [] _ _ _ Nothing = error "Should be impossible in this task"
findStartEnd (row : nextRows) rowIdx Nothing possibleStarts Nothing =
    let
        startIdx = fromList <$> sequence [Just rowIdx, checkStart row]
        endIdx = fromList <$> sequence [Just rowIdx, checkEnd row]
        possibleStartsAppend = foldl' (\x xs -> MazeCoord rowIdx xs : x) possibleStarts (findIndices (== 'a') row)
    in
        findStartEnd nextRows (rowIdx + 1) startIdx possibleStartsAppend endIdx
findStartEnd (row : nextRows) rowIdx (Just startIdx) possibleStarts Nothing =
    let
        endIdx = fromList <$> sequence [Just rowIdx, checkEnd row]
        possibleStartsAppend = foldl' (\x xs -> MazeCoord rowIdx xs : x) possibleStarts (findIndices (== 'a') row)
    in
        findStartEnd nextRows (rowIdx + 1) (Just startIdx) possibleStartsAppend endIdx
findStartEnd (row : nextRows) rowIdx Nothing possibleStarts (Just endIdx) =
    let
        startIdx = fromList <$> sequence [Just rowIdx, checkStart row]
        possibleStartsAppend = foldl' (\x xs -> MazeCoord rowIdx xs : x) possibleStarts (findIndices (== 'a') row)
    in
        findStartEnd nextRows (rowIdx + 1) startIdx possibleStartsAppend (Just endIdx)
findStartEnd (row : nextRows) rowIdx startIdx possibleStarts endIdx =
    let possibleStartsAppend = foldl' (\x xs -> MazeCoord rowIdx xs : x) possibleStarts (findIndices (== 'a') row)
    in  findStartEnd nextRows (rowIdx + 1) startIdx possibleStartsAppend endIdx

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
        fromJust . sequence $ Prelude.filter isJust [a, b, c, d]

neighborsClimbOK :: Maze -> STUArray s (Int, Int) Bool -> MazeCoord -> ST s [MazeCoord]
neighborsClimbOK maze mazeBool coord =
    filterM
        ( \v -> do
            isVisited <- visitedCheck mazeBool v
            return $ (elevationAt maze v <= elevationAt maze coord + 1) && not isVisited
        )
        $ neighbours coord maze

neighborsClimbOKReversed :: Maze -> STUArray s (Int, Int) Bool -> MazeCoord -> ST s [MazeCoord]
neighborsClimbOKReversed maze mazeBool coord =
    filterM
        ( \v -> do
            isVisited <- visitedCheck mazeBool v
            return $ (elevationAt maze coord <= elevationAt maze v + 1) && not isVisited
        )
        $ neighbours coord maze

manhattanDistance ::
    -- current coordinate
    MazeCoord ->
    -- desired end coordinate
    MazeCoord ->
    Int
manhattanDistance currCoord@(MazeCoord x1 y1) (MazeCoord xEnd yEnd) = abs (xEnd - x1) + abs (yEnd - y1)

-- Update visited status in O(1)
setVisited :: STUArray s (Int, Int) Bool -> MazeCoord -> ST s ()
setVisited mazeBool (MazeCoord r c) =
    writeArray mazeBool (r, c) True

-- Update gscores in O(1)
setGScore :: STArray s (Int, Int) (Maybe Int) -> MazeCoord -> Int -> ST s ()
setGScore grid (MazeCoord r c) score =
    writeArray grid (r, c) (Just score)

aStar ::
    -- start node
    MazeCoord ->
    -- end node
    MazeCoord ->
    -- Maze
    Maze ->
    -- return path length
    Maybe Int
aStar startNode endNode maze@(Maze _ maxRows maxColumns) = runST $ do
    mazeBool <-
        newArray ((0, 0), (maxRows - 1, maxColumns - 1)) False :: ST s (STUArray s (Int, Int) Bool)
    gScores <-
        newArray ((0, 0), (maxRows - 1, maxColumns - 1)) Nothing :: ST s (STArray s (Int, Int) (Maybe Int))

    setGScore gScores startNode 0

    -- Initial f-score
    let initialFScore = manhattanDistance startNode endNode

    aStar' (PSQ.singleton startNode initialFScore) mazeBool gScores
  where
    aStar' ::
        -- priority queue of nodes with priority of gscore+fscore
        PSQ.PSQ MazeCoord Int ->
        -- visited nodes bool array
        STUArray s (Int, Int) Bool ->
        -- g-scores
        STArray s (Int, Int) (Maybe Int) ->
        -- return path length
        ST s (Maybe Int)
    aStar' pqNodes mazeBool gScores
        | PSQ.null pqNodes = return Nothing
        | otherwise = do
            let currNode = PSQ.key . fromJust $ PSQ.findMin pqNodes
            let pqNodesRest = PSQ.deleteMin pqNodes

            currGMaybe <- gScoreAt gScores currNode
            let currG = fromJust currGMaybe

            if currNode == endNode
                then return $ Just currG
                else do
                    setVisited mazeBool currNode
                    validNeighbors <- neighborsClimbOK maze mazeBool currNode

                    neighborScores <-
                        mapM
                            ( \node -> do
                                let gScore = currG + 1
                                let hScore = manhattanDistance node endNode
                                oldGScore <- gScoreAt gScores node
                                let isEmpty = isNothing oldGScore
                                let shouldUpdate = isEmpty || gScore < fromJust oldGScore
                                return (gScore, hScore, node, isEmpty, shouldUpdate)
                            )
                            validNeighbors

                    -- Filter to only valid updates
                    let validUpdates = filter (\(_, _, _, _, shouldUpdate) -> shouldUpdate) neighborScores

                    -- Update g-scores
                    mapM_ (\(g, _, n, _, _) -> setGScore gScores n g) validUpdates

                    -- Add neighbors to priority queue with f-score = g-score + h-score
                    let newPQ =
                            foldl'
                                ( \pq (g, h, n, isEmpty, _) ->
                                    if isEmpty
                                        then PSQ.insert n (g + h) pq
                                        else PSQ.update (\_ -> Just $ g + h) n pq
                                )
                                pqNodesRest
                                validUpdates

                    aStar' newPQ mazeBool gScores

type QueueMaze = DS.Seq MazeCoord

popQueue :: QueueMaze -> (QueueMaze, MazeCoord)
popQueue queue = case DS.viewl queue of
    DS.EmptyL -> error "Should not happen herer" -- should not happend in aoc task
    coord DS.:< rest -> (rest, coord)

bfs ::
    -- start node
    MazeCoord ->
    -- end node
    [MazeCoord] ->
    -- Maze
    Maze ->
    -- return path length
    Maybe [Int]
bfs startNode endNode maze@(Maze _ maxRows maxColumns) = runST $ do
    mazeBool <-
        newArray ((0, 0), (maxRows - 1, maxColumns - 1)) False :: ST s (STUArray s (Int, Int) Bool)
    gScores <-
        newArray ((0, 0), (maxRows - 1, maxColumns - 1)) Nothing :: ST s (STArray s (Int, Int) (Maybe Int))

    setGScore gScores startNode 0

    bfs' [] (DS.empty DS.|> startNode) mazeBool gScores
  where
    bfs' ::
        -- resulted distances
        [Int] ->
        -- queue
        QueueMaze ->
        -- visited nodes bool array
        STUArray s (Int, Int) Bool ->
        -- g-scores
        STArray s (Int, Int) (Maybe Int) ->
        -- return path lengths
        ST s (Maybe [Int])
    bfs' resDist pqNodes mazeBool gScores
        | DS.null pqNodes = return $ Just resDist
        | otherwise = do
            let (pqNodesRest, currNode) = popQueue pqNodes
            setVisited mazeBool currNode

            currGMaybe <- gScoreAt gScores currNode
            let currG = fromJust currGMaybe

            resDist <- if currNode `elem` endNode then return $ currG : resDist else return $ resDist
            validNeighbors <- neighborsClimbOKReversed maze mazeBool currNode

            neighborScores <-
                mapM
                    ( \node -> do
                        let gScore = currG + 1
                        return (gScore, node)
                    )
                    validNeighbors

            mapM_ (\(g, n) -> setGScore gScores n g) neighborScores
            mapM_ (\(g, n) -> setVisited mazeBool n) neighborScores

            -- Add neighbors to queue
            let newPQ =
                    foldl'
                        ( \pq (g, n) ->
                            pq DS.|> n
                        )
                        pqNodesRest
                        neighborScores
            bfs' resDist newPQ mazeBool gScores

day12 :: IO ()
day12 = do
    txt <- readFile "./inputs/day12.txt"
    (indexes, maze) <- return . parse $ txt
    indexes <- return . fromJust $ indexes
    let (start, possibleStarts, end) = indexes

    startTime <- getCPUTime
    let res = aStar start end maze
    case res of
        Nothing -> putStrLn "No path found"
        Just pathLength -> putStrLn $ "Part 1: " ++ show pathLength
    endTime <- getCPUTime
    putStrLn $ "Evaluation time of part 1 A*: " ++ show (fromIntegral (endTime - startTime) / 1e12)

    startTime <- getCPUTime
    let res = bfs end [start] maze
    case res of
        Nothing -> putStrLn "BFS;No path found"
        Just pathLength -> putStrLn $ "BFS;Part 1: " ++ show pathLength
    endTime <- getCPUTime
    putStrLn $ "Evaluation time of part 1 BFS: " ++ show (fromIntegral (endTime - startTime) / 1e12)

    -- Grid is too small. Because of the verhead actual time is worse with parMap compared to map
    -- let res2 = sequence . filter isJust $ parMap rdeepseq (\start -> aStar start end maze) possibleStarts
    startTime <- getCPUTime
    let res2 = sequence . filter isJust $ map (\s -> aStar s end maze) possibleStarts
    -- Left here as example of bang patterns
    -- let res2 = sequence . filter isJust $ map (\s -> let !r = aStar s end maze in r) possibleStarts
    putStrLn $ "Part 2: " ++ show (fmap minimum res2)
    endTime <- getCPUTime
    putStrLn $ "Evaluation time of part 2 A*: " ++ show (fromIntegral (endTime - startTime) / 1e12)

    startTime <- getCPUTime
    let res2bfs = bfs end possibleStarts maze
    putStrLn $ "BFS;Part 2: " ++ show (fmap minimum res2bfs)
    endTime <- getCPUTime
    putStrLn $ "Evaluation time of part 2 BFS: " ++ show (fromIntegral (endTime - startTime) / 1e12)
