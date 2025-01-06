{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day10 (day10) where

import Control.Applicative (many, (*>), (<*))
import Text.Megaparsec (
    Parsec,
    eof,
    parseMaybe,
    (<|>),
 )

import Control.Lens (makeLenses, (&), (.~), Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), State, evalState, evalStateT, gets, runState, runStateT, StateT, state)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Control.Monad.IO.Class (liftIO)

data Instruction = Noop | Addx Int
    deriving (Show, Eq, Ord)
newtype CycleVal = CycleVal (Int, Int)
type Parser = Parsec Void String

data TaskState10 = TaskState10
    { _timerCount :: Int
    , _regCount :: Int
    }
    deriving (Show)

makeLenses ''TaskState10

oneLineSequence :: Parser Instruction
oneLineSequence =
    (Noop <$ (string "noop" <* many newline)) <|> Addx
        <$> ((string "addx ") *> (signed (return ()) decimal) <* many newline)

oneInstructionProcessing :: Instruction -> State TaskState10 Int
oneInstructionProcessing Noop = do
    curr_state <- get
    let new_timer_count = _timerCount curr_state + 1
    let new_state = curr_state & (timerCount .~ new_timer_count)
    put new_state
    return $ _regCount curr_state
oneInstructionProcessing (Addx val) = do
    curr_state <- get
    let new_timer_count = _timerCount curr_state + 2
    let new_reg_count = _regCount curr_state + val
    let new_state =
            curr_state
                & (timerCount .~ new_timer_count)
                & (regCount .~ new_reg_count)
    put new_state
    return new_reg_count

instructionsProcessing :: [Instruction] -> Int -> Int -> StateT TaskState10 IO Int
instructionsProcessing [] _ accum = return $ accum
instructionsProcessing (instruction : instrscs) next_timer accum = do
    prev_state <- get
    state . runState $ oneInstructionProcessing instruction
    curr_state <- get
    let current_timer = _timerCount curr_state
    liftIO $ print "Yay"
    if (current_timer + 1) == next_timer
        then
            instructionsProcessing instrscs (next_timer + 40) (accum + next_timer * (_regCount curr_state))
        else
            if (current_timer + 1) > next_timer
                then
                    instructionsProcessing instrscs (next_timer + 40) (accum + next_timer * (_regCount prev_state))
                else
                    instructionsProcessing instrscs next_timer accum

day10 :: IO ()
day10 = do
    inputs <- fromJust <$> parseMaybe (many oneLineSequence <* eof) <$> (readFile "./task_10.txt")
    let initialStatePart2 =
            TaskState10
                { _timerCount = 0
                , _regCount = 1
                }
    answer <- evalStateT (instructionsProcessing inputs 20 0) initialStatePart2
    print answer
