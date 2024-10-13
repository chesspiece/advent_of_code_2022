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

data TaskState = TaskState
    { _coord_head :: Coordinate
    , _coord_tail :: Coordinate
    , _check_visited :: HM.Map Coordinate Bool
    , _count :: Int
    }
    deriving (Show)

makeLenses ''TaskState

newState :: String -> Int -> Coordinate -> Coordinate
newState "U" num_steps coord = (fst coord, snd coord + num_steps)
newState "D" num_steps coord = (fst coord, snd coord - num_steps)
newState "R" num_steps coord = (fst coord + num_steps, snd coord)
newState "L" num_steps coord = (fst coord - num_steps, snd coord)

stateProc :: [Instruction] -> State TaskState Int
stateProc [] =
    fmap _count get
stateProc instructions = do
    curr_state <- get
    let move = fst $ head instructions
    let count = snd $ head instructions
    put $ curr_state & coord_head .~ newState move count (_coord_head curr_state)
    -- need to process tail coordinates
    stateProc (tail instructions)

day9 :: IO ()
day9 = do
    inputs <- readFile "./task_9.txt" >>= return . map ((\x -> (head x, read (x !! 1) :: Integer)) . splitOn " ") . lines
    let initilState =
            TaskState
                { _coord_head = (0, 0)
                , _coord_tail = (0, 0)
                , _check_visited = HM.singleton (0, 0) True
                , _count = 1
                }
    print "Test"
