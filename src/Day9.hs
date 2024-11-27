{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day9 (day9) where

import Control.Lens (makeLenses, (&), (.~))

import Control.Arrow (Arrow (second))
import Control.Monad.State (MonadState (get, put), State, evalState, gets, runState)
import Data.Char (digitToInt)
import qualified Data.HashMap as HM
import qualified Data.List.NonEmpty as DL
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace

type Instruction = (String, Int)
type Coordinate = (Int, Int)
newtype Metric = Metric Int
    deriving (Show)

data TaskState = TaskState
    { _coord_head :: Coordinate
    , _coord_tail :: Coordinate
    , _check_visited :: HM.Map Coordinate Bool
    , _count :: Int
    }
    deriving (Show)

data TaskStatePart2 = TaskStatePart2
    { _coord_head_part2 :: [Coordinate]
    , _last_knot_coord_part2 :: Coordinate
    , _coord_tail_part2 :: Coordinate
    , _check_visited_part2 :: HM.Map Coordinate Bool
    , _count_part2 :: Int
    }
    deriving (Show)

data Direction = None | CurrentHead | Up | Down | Left | Right | RightUp | RightDown | LeftUp | LeftDown
    deriving (Eq, Show)

makeLenses ''TaskState
makeLenses ''TaskStatePart2

newCoordHead :: String -> Int -> Coordinate -> Coordinate
newCoordHead "U" num_steps coord = (fst coord, snd coord + num_steps)
newCoordHead "D" num_steps coord = (fst coord, snd coord - num_steps)
newCoordHead "R" num_steps coord = (fst coord + num_steps, snd coord)
newCoordHead "L" num_steps coord = (fst coord - num_steps, snd coord)

newCoordHeadPart2 :: String -> Int -> Coordinate -> Coordinate
newCoordHeadPart2 "U" num_steps coord = (fst coord, snd coord + num_steps)
newCoordHeadPart2 "D" num_steps coord = (fst coord, snd coord - num_steps)
newCoordHeadPart2 "R" num_steps coord = (fst coord + num_steps, snd coord)
newCoordHeadPart2 "L" num_steps coord = (fst coord - num_steps, snd coord)

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
tailDirMap (x, y)
    | x > 0 && y > 0 = RightUp
    | x < 0 && y < 0 = LeftDown
    | x > 0 && y < 0 = RightDown
    | x < 0 && y > 0 = LeftUp

tailMoveOneStep :: Direction -> Coordinate -> Coordinate
tailMoveOneStep None a = a
tailMoveOneStep CurrentHead a = a
tailMoveOneStep Up (x, y) = (x, y + 1)
tailMoveOneStep Down (x, y) = (x, y - 1)
tailMoveOneStep Day9.Right (x, y) = (x + 1, y)
tailMoveOneStep Day9.Left (x, y) = (x - 1, y)
tailMoveOneStep RightUp (x, y) = (x + 1, y + 1)
tailMoveOneStep RightDown (x, y) = (x + 1, y - 1)
tailMoveOneStep LeftUp (x, y) = (x - 1, y + 1)
tailMoveOneStep LeftDown (x, y) = (x - 1, y - 1)

knotMove :: Direction -> Metric -> Coordinate -> Coordinate
knotMove None _ a = a
knotMove CurrentHead _ a = a
knotMove Up (Metric m) (x, y) = (x, y + m - 1)
knotMove Down (Metric m) (x, y) = (x, y - m + 1)
knotMove Day9.Right (Metric m) (x, y) = (x + m - 1, y)
knotMove Day9.Left (Metric m) (x, y) = (x - m + 1, y)
knotMove RightUp (Metric 2) (x, y) = (x + 1, y + 1)
knotMove RightDown (Metric 2) (x, y) = (x + 1, y - 1)
knotMove LeftUp (Metric 2) (x, y) = (x - 1, y + 1)
knotMove LeftDown (Metric 2) (x, y) = (x - 1, y - 1)
knotMove RightUp (Metric m) (x, y) = knotMove Up (Metric (m - 1)) (x + 1, y + 1)
knotMove RightDown (Metric m) (x, y) = knotMove Down (Metric (m - 1)) (x + 1, y - 1)
knotMove LeftUp (Metric m) (x, y) = knotMove LeftUp (Metric (m - 1)) (x - 1, y + 1)
knotMove LeftDown (Metric m) (x, y) = knotMove LeftDown (Metric (m - 1)) (x - 1, y - 1)

processKnots :: [Coordinate] -> [Coordinate] -> ([Coordinate], Coordinate)
processKnots (x : []) coord_list =
    let
        x2 = knotMove (tailDirMap (coordMinus (head coord_list) x)) (coordMetric (head coord_list) x) x
        coord_list2 = x2 : coord_list
     in
        (coord_list2, x)
processKnots (x : xs) [] = processKnots xs [x]
processKnots (x : xs) coord_list =
    let
        x2 = knotMove (tailDirMap (coordMinus (head coord_list) x)) (coordMetric (head coord_list) x) x
        coord_list2 = x2 : coord_list
     in
        processKnots xs coord_list2

stateProcPart2 :: [Instruction] -> State TaskStatePart2 Int
stateProcPart2 [] = do
    _count_part2 <$> get
stateProcPart2 instructions = do
    let move = fst $ head instructions
    let count_move = snd $ head instructions
    curr_state <- get

    let new_coord_head = newCoordHead move count_move (head $ _coord_head_part2 curr_state)
    let (a, b) = processKnots (new_coord_head : (tail $ _coord_head_part2 curr_state)) []
    let new_state = curr_state & (coord_head_part2 .~ a) & (last_knot_coord_part2 .~ b)
    put new_state
    -- traceShowM a
    -- computeTailDirectionPart2 $ traceShow (coordMetric b (_coord_tail_part2 curr_state)) (Metric 1)
    stateProcPart2 (tail instructions)

-- _count_part2 <$> get

computeTailDirection :: Metric -> State TaskState Int
computeTailDirection (Metric 1) = fmap _count get
computeTailDirection _ = do
    curr_state <- get
    let curr_check_visited = _check_visited curr_state
    let curr_count = _count curr_state

    let tail_direction = tailDirMap $ coordMinus (_coord_head curr_state) (_coord_tail curr_state)
    let new_tail_coord = tailMoveOneStep tail_direction (_coord_tail curr_state)

    let check_coord = if HM.member new_tail_coord curr_check_visited then 0 else 1

    let new_state =
            curr_state
                & (check_visited .~ (HM.insert new_tail_coord True curr_check_visited))
                & (count .~ (curr_count + check_coord))
                & (coord_tail .~ new_tail_coord)
    put new_state

    let metric = coordMetric (_coord_head curr_state) new_tail_coord
    computeTailDirection metric

stateProc :: [Instruction] -> State TaskState Int
stateProc [] = do
    _count <$> get
stateProc instructions = do
    let move = fst $ head instructions
    let count_move = snd $ head instructions
    curr_state <- get
    let new_coord_head = newCoordHead move count_move (_coord_head curr_state)

    let new_state = curr_state & (coord_head .~ new_coord_head)
    put new_state

    computeTailDirection $ coordMetric new_coord_head (_coord_tail curr_state)
    stateProc (tail instructions)

day9 :: IO ()
day9 = do
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
    print count
    let initialStatePart2 =
            TaskStatePart2
                { _coord_head_part2 = [(0, 0) | _ <- [0 .. 8]]
                , _last_knot_coord_part2 = (0, 0)
                , _coord_tail_part2 = (0, 0)
                , _check_visited_part2 = HM.singleton (0, 0) True
                , _count_part2 = 1
                }
    let count = evalState (stateProcPart2 inputs) initialStatePart2
    print count
