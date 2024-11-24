{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day9 (day9) where

import Control.Lens

import Control.Arrow (Arrow (second))
import Control.Monad.State (MonadState (get, put), State, evalState, gets, runState)
import Data.Char (digitToInt)
import qualified Data.HashMap as HM
import qualified Data.List.NonEmpty as DL
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Vector as V

type Instruction = (String, Int)
type Coordinate = (Int, Int)
newtype Metric = Metric Int

data TaskState = TaskState
    { _coord_head :: Coordinate
    , _coord_tail :: Coordinate
    , _check_visited :: HM.Map Coordinate Bool
    , _count :: Int
    }
    deriving (Show)

data Direction = None | CurrentHead | Up | Down | Left | Right | RightUp | RightDown | LeftUp | LeftDown
    deriving (Eq)

makeLenses ''TaskState

newCoordHead :: String -> Int -> Coordinate -> Coordinate
newCoordHead "U" num_steps coord = (fst coord, snd coord + num_steps)
newCoordHead "D" num_steps coord = (fst coord, snd coord - num_steps)
newCoordHead "R" num_steps coord = (fst coord + num_steps, snd coord)
newCoordHead "L" num_steps coord = (fst coord - num_steps, snd coord)

coordMinus :: Coordinate -> Coordinate -> Coordinate
coordMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

coordMetric :: Coordinate -> Coordinate -> Metric
coordMetric (x1, y1) (x2, y2) = Metric $ max (max (abs (x1 - x2)) (abs (y1 - y2))) 1

tailDirMap :: Coordinate -> Direction
tailDirMap (1, 1) = None
tailDirMap (0, 0) = CurrentHead
tailDirMap (0, y)
    | y > 0 = Up
    | y < 0 = Down
tailDirMap (x, 0)
    | x > 0 = Day9.Right
    | x < 0 = Day9.Left
tailDirMap (1, y)
    | y > 0 = RightUp
    | y < 0 = RightDown
tailDirMap (-1, y)
    | y > 0 = LeftUp
    | y < 0 = LeftDown
tailDirMap (x, 1)
    | x > 0 = RightUp
    | x < 0 = LeftUp
tailDirMap (x, -1)
    | x > 0 = RightDown
    | x < 0 = LeftDown
tailDirMap (_, _) = error "Should be impossible in this task"

tailMoveOneStep :: Direction -> Coordinate -> Coordinate
tailMoveOneStep None a = a
tailMoveOneStep CurrentHead a = a
tailMoveOneStep Up (x, y) = (x, y + 1)
tailMoveOneStep Down (x, y) = (x, y - 1)
tailMoveOneStep Day9.RightUp (x, y) = (x + 1, y + 1)
tailMoveOneStep Day9.RightDown (x, y) = (x + 1, y - 1)
tailMoveOneStep Day9.LeftUp (x, y) = (x - 1, y + 1)
tailMoveOneStep Day9.LeftDown (x, y) = (x - 1, y - 1)

computeTailDirection :: Metric -> State TaskState Int
computeTailDirection (Metric 1) = fmap _count get
computeTailDirection _ = do
    curr_state <- get
    let curr_check_visited = _check_visited curr_state
    let curr_count = _count curr_state

    let tail_direction = tailDirMap $ coordMinus (_coord_head curr_state) (_coord_tail curr_state)
    let new_tail_coord = tailMoveOneStep tail_direction (_coord_tail curr_state)

    let check_coord = if HM.member new_tail_coord curr_check_visited then 1 else 0

    let new_state =
            curr_state
                & (check_visited .~ (HM.insert new_tail_coord True curr_check_visited))
                & (count .~ (curr_count + check_coord))
                & (coord_tail .~ new_tail_coord)

    let metric = coordMetric (_coord_head curr_state) (_coord_tail curr_state)
    computeTailDirection metric

-- newStateTail :: Coordinate -> Coordinate -> Coordinate
-- newStateTail "U" coord = (fst coord, snd coord + num_steps)
-- newStateTail "D" coord = (fst coord, snd coord - num_steps)
-- newStateTail "R" coord = (fst coord + num_steps, snd coord)
-- newStateTail "L" coord = (fst coord - num_steps, snd coord)

stateProc :: [Instruction] -> State TaskState Int
stateProc [] =
    fmap _count get
stateProc instructions = do
    curr_state <- get
    let move = fst $ head instructions
    let count_move = snd $ head instructions
    let new_coord_head = newCoordHead move count_move (_coord_head curr_state)
    -- need to process tail coordinates
    let new_state = curr_state & (coord_head .~ new_coord_head) & (count .~ 10)
    put new_state
    stateProc (tail instructions)

day9 :: IO ()
day9 = do
    print "test2"
    inputs <-
        readFile "./task_9.txt"
            >>= return . map ((\x -> (head x, read (x !! 1) :: Int)) . splitOn " ") . lines
    let initialState =
            TaskState
                { _coord_head = (0, 0)
                , _coord_tail = (0, 0)
                , _check_visited = HM.singleton (0, 0) True
                , _count = 1
                }
    let count = evalState (stateProc inputs) initialState
    print "test2"
    print count
