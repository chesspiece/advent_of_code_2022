{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day8 (day8) where

import Control.Lens
import Control.Monad.State (MonadState (get, put), State, evalState, gets, runState)
import Data.Char (digitToInt)

data TaskState = TaskState {_boardState :: [[Int]], _compState :: [[Int]], _maxColumns :: Int, _maxRows :: Int}
  deriving (Show)

makeLenses ''TaskState

firstRun :: Int -> Int -> State TaskState [[Int]]
firstRun i 0 = do firstRun i 1
firstRun i j = do
  currSt <- get
  if i /= _maxRows currSt || j /= _maxColumns currSt
    then do
      let choice = max ((_boardState currSt !! i) !! (j - 1)) ((_compState currSt !! i) !! (j - 1))
      let curr_sol = _compState currSt & ix i . ix j .~ choice
      put $
        TaskState
          { _boardState = _boardState currSt,
            _compState = curr_sol,
            _maxColumns = _maxColumns currSt,
            _maxRows = _maxRows currSt
          }
      if j == _maxColumns currSt
        then firstRun (i + 1) 0
        else firstRun i (j + 1)
    else do _compState <$> get

secondRun :: Int -> Int -> State TaskState [[Int]]
secondRun 0 j = do secondRun 1 0
secondRun i j = do
  currSt <- get
  if i /= _maxRows currSt || j /= _maxColumns currSt
    then do
      let choice = max ((_boardState currSt !! (i - 1)) !! j) ((_compState currSt !! (i - 1)) !! j)
      let curr_sol = _compState currSt & ix i . ix j .~ choice
      put $
        TaskState
          { _boardState = _boardState currSt,
            _compState = curr_sol,
            _maxColumns = _maxColumns currSt,
            _maxRows = _maxRows currSt
          }
      if j == _maxColumns currSt
        then secondRun (i + 1) 0
        else secondRun i (j + 1)
    else do _compState <$> get

thirdRun :: Int -> Int -> State TaskState [[Int]]
thirdRun 0 0 = _compState <$> get
thirdRun i j = do
  currSt <- get
  let checkColumns = _maxColumns currSt
  let checkRows = _maxRows currSt
  if i == checkRows
    then do
      let curr_sol = _compState currSt & ix i . ix j .~ (-1)
      put $
        TaskState
          { _boardState = _boardState currSt,
            _compState = curr_sol,
            _maxColumns = _maxColumns currSt,
            _maxRows = _maxRows currSt
          }
      if j == 0
        then thirdRun (i - 1) (_maxColumns currSt)
        else thirdRun i (j - 1)
    else do
      let choice = max ((_boardState currSt !! (i + 1)) !! j) ((_compState currSt !! (i + 1)) !! j)
      let curr_sol = _compState currSt & ix i . ix j .~ choice
      put $
        TaskState
          { _boardState = _boardState currSt,
            _compState = curr_sol,
            _maxColumns = _maxColumns currSt,
            _maxRows = _maxRows currSt
          }
      if j == 0
        then thirdRun (i - 1) (_maxColumns currSt)
        else thirdRun i (j - 1)

forthRun :: Int -> Int -> State TaskState [[Int]]
forthRun 0 0 = _compState <$> get
forthRun i j = do
  currSt <- get
  let checkColumns = _maxColumns currSt
  let checkRows = _maxRows currSt
  if j == checkColumns
    then do
      let curr_sol = _compState currSt & ix i . ix j .~ (-1)
      put $
        TaskState
          { _boardState = _boardState currSt,
            _compState = curr_sol,
            _maxColumns = _maxColumns currSt,
            _maxRows = _maxRows currSt
          }
      forthRun i (j - 1)
    else do
      let choice = max ((_boardState currSt !! i) !! (j + 1)) ((_compState currSt !! i) !! (j + 1))
      let curr_sol = _compState currSt & ix i . ix j .~ choice
      put $
        TaskState
          { _boardState = _boardState currSt,
            _compState = curr_sol,
            _maxColumns = _maxColumns currSt,
            _maxRows = _maxRows currSt
          }
      if j == 0
        then forthRun (i - 1) (_maxColumns currSt)
        else forthRun i (j - 1)

day8 :: IO ()
day8 = do
  input_stream <- readFile "./task_8.txt" >>= return . map (map digitToInt) . lines
  let s1 = length input_stream
  let s2 = length $ head input_stream
  let initial_sol = replicate s1 $ replicate s2 (-1)
  let task = TaskState {_boardState = input_stream, _compState = initial_sol, _maxColumns = s1 - 1, _maxRows = s2 - 1}
  let firstHalf = evalState (firstRun 0 0) task
  let secondHalf = evalState (secondRun 0 0) task
  let thirdHalf = evalState (thirdRun (s1 -1) (s2 - 1)) task
  let fourthHalf = evalState (forthRun (s1 - 1) (s2 - 1)) task
  let firstResHalf = [[ min x1 y1 | (x1, y1) <- zip x y] | (x, y) <- zip firstHalf secondHalf]
  let secondResHalf = [[ min x1 y1 | (x1, y1) <- zip x y] | (x, y) <- zip firstResHalf thirdHalf]
  let thirdResHalf = [[ min x1 y1 | (x1, y1) <- zip x y] | (x, y) <- zip secondResHalf fourthHalf]
  let res = [[ if x1 < y1 then 1 else 0 | (x1, y1) <- zip x y] | (x, y) <- zip thirdResHalf input_stream]
  print $ sum $ map sum res
