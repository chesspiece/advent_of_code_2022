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

import Control.Lens (Identity (runIdentity), makeLenses, (&), (.~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (
    MonadState (get, put),
    State,
    StateT,
    evalState,
    evalStateT,
    gets,
    runState,
    runStateT,
    state,
 )
import Data.Maybe (fromJust)
import Data.Void (Void)
import Debug.Trace (trace, traceIO, traceM)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Instruction = Noop | Addx Int
    deriving (Show, Eq, Ord)
newtype CycleVal = CycleVal (Int, Int)
type Parser = Parsec Void String

data TaskState = TaskState10
    { _timerCount :: Int
    , _regCount :: Int
    }
    deriving (Show)

makeLenses ''TaskState

oneLineSequence :: Parser Instruction
oneLineSequence =
    (Noop <$ (string "noop" <* many newline)) <|> Addx
        <$> ((string "addx ") *> (signed (return ()) decimal) <* many newline)

oneInstructionProcessing :: Instruction -> State TaskState Int
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

checkIfVisible :: Int -> Int -> Bool
checkIfVisible pos spritePos
    | pos == spritePos || pos == (spritePos - 1) || pos == (spritePos + 1) = True
    | otherwise = False

jumpLine :: Int -> IO ()
jumpLine n
    | mod n 40 == 0 = putStr "\n"
    | otherwise = return ()

printSprites :: Bool -> Int -> Int -> Int -> IO ()
printSprites True 1 tmr spritePos = jumpLine tmr >> putStr "#"
printSprites False 1 tmr spritePos = jumpLine tmr >> putStr "."
printSprites True n tmr spritePos = do
    jumpLine tmr
    putStr "#"
    -- traceIO (show n)
    printSprites (checkIfVisible (mod tmr 40) spritePos) (n - 1) (tmr + 1) spritePos
printSprites False n tmr spritePos = do
    jumpLine tmr
    putStr "."
    printSprites (checkIfVisible (mod tmr 40) spritePos) (n - 1) (tmr + 1) spritePos

instructionsProcessing :: [Instruction] -> Int -> Int -> StateT TaskState IO Int
instructionsProcessing [] _ accum = return $ accum
instructionsProcessing (instruction : instrscs) next_timer accum = do
    prev_state <- get
    state . runState $ oneInstructionProcessing instruction
    curr_state <- get
    let prev_timer = _timerCount prev_state
    let spritePos = _regCount prev_state
    let current_timer = _timerCount curr_state
    liftIO $
        printSprites
            (checkIfVisible (mod prev_timer 40) spritePos)
            (current_timer - prev_timer)
            prev_timer
            spritePos
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
    putStr "\n"
    print answer
