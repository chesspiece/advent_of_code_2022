{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day8 (day8) where

import Control.Lens
import Control.Monad.State (MonadState (get, put), State, evalState, gets, runState, state)
import Data.Char (digitToInt)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.IO.Strict as S

data TaskState = TaskState
    { _boardState :: V.Vector (V.Vector Int)
    , _compState :: V.Vector (V.Vector Int)
    , _maxColumns :: Int
    , _maxRows :: Int
    , _helperVec :: V.Vector Int
    }
    deriving (Show)

makeLenses ''TaskState

firstRun :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
firstRun i 0 = do firstRun i 1
firstRun i j = do
    currSt <- get
    if i /= _maxRows currSt || j /= _maxColumns currSt
        then do
            let choice = max ((_boardState currSt V.! i) V.! (j - 1)) ((_compState currSt V.! i) V.! (j - 1))
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ currSt & compState .~ curr_sol
            if j == _maxColumns currSt
                then firstRun (i + 1) 0
                else firstRun i (j + 1)
        else do _compState <$> get

secondRun :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
secondRun 0 j = do secondRun 1 0
secondRun i j = do
    currSt <- get
    if i /= _maxRows currSt || j /= _maxColumns currSt
        then do
            let choice = max ((_boardState currSt V.! (i - 1)) V.! j) ((_compState currSt V.! (i - 1)) V.! j)
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ currSt & compState .~ curr_sol
            if j == _maxColumns currSt
                then secondRun (i + 1) 0
                else secondRun i (j + 1)
        else do _compState <$> get

thirdRun :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
thirdRun 0 0 = _compState <$> get
thirdRun i j = do
    currSt <- get
    let checkColumns = _maxColumns currSt
    let checkRows = _maxRows currSt
    if i == checkRows
        then do
            let curr_sol = _compState currSt & ix i . ix j .~ (-1)
            put $ currSt & compState .~ curr_sol
            if j == 0
                then thirdRun (i - 1) (_maxColumns currSt)
                else thirdRun i (j - 1)
        else do
            let choice = max ((_boardState currSt V.! (i + 1)) V.! j) ((_compState currSt V.! (i + 1)) V.! j)
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ currSt & compState .~ curr_sol
            if j == 0
                then thirdRun (i - 1) (_maxColumns currSt)
                else thirdRun i (j - 1)

fourthRun :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
fourthRun 0 0 = _compState <$> get
fourthRun i j = do
    currSt <- get
    let checkColumns = _maxColumns currSt
    let checkRows = _maxRows currSt
    if j == checkColumns
        then do
            let curr_sol = _compState currSt & ix i . ix j .~ (-1)
            put $ currSt & compState .~ curr_sol
            fourthRun i (j - 1)
        else do
            let choice = max ((_boardState currSt V.! i) V.! (j + 1)) ((_compState currSt V.! i) V.! (j + 1))
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ currSt & compState .~ curr_sol
            if j == 0
                then fourthRun (i - 1) (_maxColumns currSt)
                else fourthRun i (j - 1)

fillHelperVector :: V.Vector Int -> Int -> Int -> V.Vector Int
fillHelperVector vec val pos = vec V.// [(i, pos) | i <- [0 .. val]]

firstRun2 :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
firstRun2 i j = do
    currSt <- get
    if i /= _maxRows currSt || j /= _maxColumns currSt
        then do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = j - (_helperVec currSt V.! tree_height)
            let new_vec = fillHelperVector (_helperVec currSt) tree_height j
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            if j == _maxColumns currSt
                then do
                    put $ currSt & helperVec .~ V.replicate 10 0
                    firstRun2 (i + 1) 0
                else firstRun2 i (j + 1)
        else do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = j - (_helperVec currSt V.! tree_height)
            let new_vec = fillHelperVector (_helperVec currSt) tree_height j
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            _compState <$> get

secondRun2 :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
secondRun2 i j = do
    currSt <- get
    if i /= _maxRows currSt || j /= _maxColumns currSt
        then do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = i - (_helperVec currSt V.! tree_height)
            let new_vec = fillHelperVector (_helperVec currSt) tree_height i
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            if i == _maxRows currSt
                then do
                    put $ currSt & helperVec .~ V.replicate 10 0
                    secondRun2 0 (j + 1)
                else secondRun2 (i + 1) j
        else do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = i - (_helperVec currSt V.! tree_height)
            let new_vec = fillHelperVector (_helperVec currSt) tree_height i
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            _compState <$> get

thirdRun2 :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
thirdRun2 i j = do
    currSt <- get
    if i /= 0 || j /= 0
        then do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = (_helperVec currSt V.! tree_height) - j
            let new_vec = fillHelperVector (_helperVec currSt) tree_height j
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            if j == 0
                then do
                    put $ currSt & helperVec .~ V.replicate 10 (_maxColumns currSt)
                    thirdRun2 (i - 1) (_maxColumns currSt)
                else thirdRun2 i (j - 1)
        else do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = (_helperVec currSt V.! tree_height) - j
            let new_vec = fillHelperVector (_helperVec currSt) tree_height j
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            _compState <$> get

fourthRun2 :: Int -> Int -> State TaskState (V.Vector (V.Vector Int))
fourthRun2 i j = do
    currSt <- get
    if i /= 0 || j /= 0
        then do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = (_helperVec currSt V.! tree_height) - i
            let new_vec = fillHelperVector (_helperVec currSt) tree_height i
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            if i == 0
                then do
                    put $ currSt & helperVec .~ V.replicate 10 (_maxRows currSt)
                    fourthRun2 (_maxRows currSt) (j - 1)
                else fourthRun2 (i - 1) j
        else do
            let tree_height = (_boardState currSt V.! i) V.! j
            let choice = (_helperVec currSt V.! tree_height) - i
            let new_vec = fillHelperVector (_helperVec currSt) tree_height i
            let curr_sol = _compState currSt & ix i . ix j .~ choice
            put $ (currSt & compState .~ curr_sol) & helperVec .~ new_vec
            _compState <$> get

day8 :: IO ()
day8 = do
    input_stream <- S.readFile "./task_8.txt" >>= return . V.fromList . map (V.fromList . map digitToInt) . lines
    let s1 = length input_stream
    let s2 = length $ input_stream V.! 0
    let initial_sol = V.replicate s1 $ V.replicate s2 (-1)
    let initial_vec = V.replicate 10 0
    let initial_vec_rev_rows = V.replicate 10 (s1 - 1)
    let initial_vec_rev_columns = V.replicate 10 (s2 - 1)

    let task =
            TaskState
                { _boardState = input_stream
                , _compState = initial_sol
                , _maxColumns = s1 - 1
                , _maxRows = s2 - 1
                , _helperVec = initial_vec
                }
    let firstPart = V.toList . V.map V.toList $ evalState (firstRun 0 0) task
    let secondPart = V.toList . V.map V.toList $ evalState (secondRun 0 0) task
    let thirdPart = V.toList . V.map V.toList $ evalState (thirdRun (s1 - 1) (s2 - 1)) task
    let fourthPart = V.toList . V.map V.toList $ evalState (fourthRun (s1 - 1) (s2 - 1)) task
    let firstResPart = [[min x1 y1 | (x1, y1) <- zip x y] | (x, y) <- zip firstPart secondPart]
    let secondResPart = [[min x1 y1 | (x1, y1) <- zip x y] | (x, y) <- zip firstResPart thirdPart]
    let thirdResPart = [[min x1 y1 | (x1, y1) <- zip x y] | (x, y) <- zip secondResPart fourthPart]
    let res = [[if x1 < y1 then 1 else 0 | (x1, y1) <- zip x y] | (x, y) <- zip thirdResPart (V.toList . V.map V.toList $ input_stream)]
    print $ sum $ map sum res

    let initial_sol2 = V.replicate s1 $ V.replicate s2 0
    let task2 =
            TaskState
                { _boardState = input_stream
                , _compState = initial_sol2
                , _maxColumns = s1 - 1
                , _maxRows = s2 - 1
                , _helperVec = initial_vec
                }
    let task2_rev_columns =
            TaskState
                { _boardState = input_stream
                , _compState = initial_sol2
                , _maxColumns = s1 - 1
                , _maxRows = s2 - 1
                , _helperVec = initial_vec_rev_columns
                }
    let task2_rev_rows =
            TaskState
                { _boardState = input_stream
                , _compState = initial_sol2
                , _maxColumns = s1 - 1
                , _maxRows = s2 - 1
                , _helperVec = initial_vec_rev_rows
                }
    let firstPart2 = V.toList . V.map V.toList $ evalState (firstRun2 0 0) task2
    let secondPart2 = V.toList . V.map V.toList $ evalState (secondRun2 0 0) task2
    let thirdPart2 = V.toList . V.map V.toList $ evalState (thirdRun2 (s1 - 1) (s2 - 1)) task2_rev_columns
    let fourthPart2 = V.toList . V.map V.toList $ evalState (fourthRun2 (s1 - 1) (s2 - 1)) task2_rev_rows
    let firstResPart2 = [[x1 * y1 | (x1, y1) <- zip x y] | (x, y) <- zip firstPart2 secondPart2]
    let secondResPart2 = [[x1 * y1 | (x1, y1) <- zip x y] | (x, y) <- zip firstResPart2 thirdPart2]
    let res = [[x1 * y1 | (x1, y1) <- zip x y] | (x, y) <- zip secondResPart2 fourthPart2]
    print $ maximum $ map maximum res
