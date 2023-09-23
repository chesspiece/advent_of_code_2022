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
firstRun 0 0 = do
  _compState <$> get
firstRun 0 j = do
  currSt <- get
  let curr_sol = _compState currSt & ix 0 . ix j .~ (head (_boardState currSt) !! (j - 1))
  put $
    TaskState
      { _boardState = _boardState currSt,
        _compState = curr_sol,
        _maxColumns = _maxColumns currSt,
        _maxRows = _maxRows currSt
      }
  if j == 8
    then firstRun 0 (j + 1)
    else firstRun 1 0

day8 :: IO ()
day8 = do
  input_stream <- readFile "./task_8.txt" >>= return . map (map digitToInt) . lines
  let s1 = length input_stream
  let s2 = length $ head input_stream
  let initial_sol = replicate s1 $ replicate s2 0
  print input_stream
  print initial_sol
