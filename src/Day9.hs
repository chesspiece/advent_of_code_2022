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

data Direction = None | CurrentHead | Vertical | Horizontal | RightVertical | LeftVertical | UpHorizaontal | DownHorizontal
    deriving (Eq)

makeLenses ''TaskState

newCoordHead :: String -> Int -> Coordinate -> Coordinate
newCoordHead "U" num_steps coord = (fst coord, snd coord + num_steps)
newCoordHead "D" num_steps coord = (fst coord, snd coord - num_steps)
newCoordHead "R" num_steps coord = (fst coord + num_steps, snd coord)
newCoordHead "L" num_steps coord = (fst coord - num_steps, snd coord)

coordMinus :: Coordinate -> Coordinate -> Coordinate
coordMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

coordCheck :: Coordinate -> Coordinate -> Metric
coordCheck (x1, y1) (x2, y2) = Metric $ abs (x1 - x2) + abs (y1 - y2)

tailDirMap :: Coordinate -> Direction
tailDirMap (1, 1) = None
tailDirMap (0, 0) = CurrentHead
tailDirMap (0, _) = Vertical
tailDirMap (_, 0) = Horizontal
tailDirMap (1, y) = RightVertical
tailDirMap (-1, y) = LeftVertical
tailDirMap (x, 1) = UpHorizaontal
tailDirMap (x, -1) = DownHorizontal
tailDirMap (_, _) = error "Should be impossible in this task"

computeTailDirection :: Metric -> Coordinate -> Coordinate -> Coordinate
computeTailDirection (Metric 0) a b = a
computeTailDirection (Metric 1) a b = a
computeTailDirection _ a b = a

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
    inputs <- readFile "./task_9.txt" >>= return . map ((\x -> (head x, read (x !! 1) :: Int)) . splitOn " ") . lines
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
