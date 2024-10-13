{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day9 (day9) where

import Control.Lens
import Control.Monad.State (MonadState (get, put), State, evalState, gets, runState)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Vector as V

type Instruction = [[Char]]

data TaskState = TaskState
    { _xCoord :: Int
    , _yCoord :: Int
    , _count :: Int
    }
    deriving (Show)

stateProc :: V.Vector Instruction -> State TaskState Int
stateProc i = do
    currSt <- get
    _count <$> get

day9 :: IO ()
day9 = do
    inputs <- readFile "./task_8.txt" >>= return . V.map (splitOn " ") . V.fromList . lines
    let initilState =
            TaskState
                { _xCoord = 0
                , _yCoord = 0
                , _count = 1
                }
    print "Test"
