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

newState :: String -> Coordinate -> Coordinate
newState "U" coord = (fst coord, snd coord + 1)
newState "D" coord = (fst coord, snd coord - 1)
newState "R" coord = (fst coord + 1, snd coord)
newState "L" coord = (fst coord - 1, snd coord)

processInstructoin :: String -> Int -> State TaskState TaskState
processInstructoin _ 0 =
    get
processInstructoin move cnt = do
    curr_state <- get
    let new_state = newState move (_coord_head curr_state) -- (fst (_coord_head curr_state), snd (_coord_head curr_state) + 1)
    put $ curr_state & coord_head .~ new_state
    processInstructoin move (cnt - 1)

stateProc :: [Instruction] -> State TaskState Int
stateProc [] =
    _count <$> get
stateProc i = do
    let move = fst $ head i
    let cnt = snd $ head i
    processInstructoin move cnt
    stateProc (tail i)

day9 :: IO ()
day9 = do
    inputs <- readFile "./task_8.txt" >>= return . map ((\x -> (head x, read (x !! 1) :: Integer)) . splitOn " ") . lines
    let initilState =
            TaskState
                { _coord_head = (0, 0)
                , _coord_tail = (0, 0)
                , _check_visited = HM.singleton (0, 0) True
                , _count = 1
                }
    print "Test"
