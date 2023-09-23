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
firstRun 4 4 = do
  _compState <$> get
firstRun 0 j = do firstRun 1 0
firstRun i 0 = do firstRun i 1
firstRun i j = do
  currSt <- get
  let choice1 = max ((_boardState currSt !! i) !! (j - 1)) ((_compState currSt !! i) !! (j - 1)) 
  let choice2 = max ((_boardState currSt !! (i - 1)) !! j) ((_compState currSt !! (i - 1)) !! j)
  let curr_sol = _compState currSt & ix i . ix j .~ min choice1 choice2
  put $
    TaskState
      { _boardState = _boardState currSt,
        _compState = curr_sol,
        _maxColumns = _maxColumns currSt,
        _maxRows = _maxRows currSt
      }
  if j == 4
    then firstRun (i + 1) 0
    else firstRun i (j + 1)

day8 :: IO ()
day8 = do
  input_stream <- readFile "./task_8.txt" >>= return . map (map digitToInt) . lines
  let s1 = length input_stream
  let s2 = length $ head input_stream
  let initial_sol = replicate s1 $ replicate s2 0
  let task = TaskState {_boardState = input_stream , _compState = initial_sol , _maxColumns = s1 - 1, _maxRows = s2 - 1}
  print $ evalState (firstRun 0 0) task
