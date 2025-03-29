{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
-- I use ParsecT monad transformer here in order to ahve an example of using Parsec subparser
-- inside of ParsecT parser.
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day11 (day11) where

import Control.Applicative (many, (*>), (<*))
import Control.Monad.Trans (lift)
import Text.Megaparsec (
    MonadParsec (getParserState, takeP),
    Parsec,
    ParsecT,
    eof,
    optional,
    runParser',
    runParserT,
    setParserState,
    skipMany,
    try,
    (<|>),
 )

import Control.Lens (makeLenses, (&), (.~))
import Control.Monad.State (
    MonadState (get, put),
    StateT,
    runState,
    runStateT,
 )

import Control.Arrow (Arrow (second))
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer as L (decimal)

data Operation where
    Mult :: Int -> Operation
    Add :: Int -> Operation
    AddOld :: Operation
    MultOld :: Operation
    deriving (Show, Eq, Ord)

type HashTable k v = H.BasicHashTable k v
type Parser = ParsecT Void String (StateT (HashTable Int MonkeyState, Int) IO)
type ParserInner = Parsec Void String

data MonkeyState = MonkeyState
    { _items :: [Int]
    , _divisibilityCheck :: Int
    , _opertion :: Operation
    , _monkeyChoice :: (Int, Int)
    , _inspectsQuantity :: Int
    }
    deriving (Show)

makeLenses ''MonkeyState

monkeyParse :: Parser ()
monkeyParse = do
    (ht, cnt) <- get
    string "Monkey "
    monkey_index <- L.decimal
    string ":"
    newline
    state <- getParserState

    let tuple_listItems = runParser' itemsParse state
    case tuple_listItems of
        (_, Left err) -> error "Parse error!"
        (_, Right monkeyList) -> do
            return ()
    let (state, Right list_items) = tuple_listItems

    let tuple_operation = runParser' operationParse state
    case tuple_operation of
        (_, Left err) -> error "Parse error! 2"
        (state, Right opr) -> do
            return ()
    let (state, Right opr) = tuple_operation

    let tuple_check = runParser' divisibleParse state
    case tuple_check of
        (_, Left err) -> error "Parse error! 3"
        (state, Right (divis, monkey1, monkey2)) -> do
            setParserState state
    let (state, Right (divis, monkey1, monkey2)) = tuple_check

    many newline
    let parsed_state =
            MonkeyState
                { _items = list_items
                , _divisibilityCheck = divis
                , _opertion = opr
                , _monkeyChoice = (monkey1, monkey2)
                , _inspectsQuantity = 0
                }
    lift . lift $ H.insert ht monkey_index parsed_state
    put (ht, cnt + 1)
    return ()

itemsParse :: ParserInner [Int]
itemsParse =
    do
        takeP Nothing 18
        -- string "  Starting items: "
        ret <- many (L.decimal <* optional (string ", "))
        newline
        return ret

operationParse :: ParserInner Operation
operationParse =
    do
        takeP Nothing 23
        -- string "  Operation: new = old "
        ret <-
            Add
                <$> try (string "+ " *> decimal)
                    <|> Mult
                <$> try (string "* " *> decimal)
                    <|> AddOld
                <$ try (string "+ old")
                    <|> MultOld
                <$ string "* old"
        newline
        return ret

divisibleParse :: ParserInner (Int, Int, Int)
divisibleParse =
    do
        takeP Nothing 21
        -- string "  Test: divisible by "
        ret <- L.decimal
        newline

        -- string "    If true: throw to monkey "
        takeP Nothing 29
        true_val <- L.decimal
        newline

        -- string "    If false: throw to monkey "
        takeP Nothing 30
        false_val <- L.decimal
        many newline
        return (ret, true_val, false_val)

applyMonkeyOperation :: Operation -> Int -> Int
applyMonkeyOperation (Mult mltp) worry = mltp * worry
applyMonkeyOperation (Add add) worry = add + worry
applyMonkeyOperation MultOld worry = worry * worry
applyMonkeyOperation AddOld worry = worry + worry

isDivisible :: Int -> Int -> Bool
isDivisible num denum
    | mod num denum == 0 = True
    | otherwise = False

chooseMonkey :: (Int, Int) -> Bool -> Int
chooseMonkey (x, y) flag
    | flag = x
    | not flag = y

monkeyAction :: Int -> [Int] -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
monkeyAction currMonkeyIdx [] monkeyTable = return monkeyTable
monkeyAction currMonkeyIdx worryList monkeyTable = do
    currentMonkeyState <- fromJust <$> H.lookup monkeyTable currMonkeyIdx
    H.insert monkeyTable currMonkeyIdx (currentMonkeyState & items .~ tail worryList)

    let newWorry = div (applyMonkeyOperation (_opertion currentMonkeyState) (head worryList)) 3
    let checkDone = isDivisible newWorry (_divisibilityCheck currentMonkeyState)
    let newMonkeyIdx = chooseMonkey (_monkeyChoice currentMonkeyState) checkDone

    nextMonkey <- fromJust <$> H.lookup monkeyTable newMonkeyIdx

    let newMonkeyItems = _items nextMonkey
    let newMonkeyState = nextMonkey & (items .~ (newMonkeyItems ++ [newWorry]))

    H.insert monkeyTable newMonkeyIdx newMonkeyState

    currentMonkeyState <- fromJust <$> H.lookup monkeyTable currMonkeyIdx

    monkeyAction currMonkeyIdx (_items currentMonkeyState) monkeyTable

runManyRounds :: Int -> Int -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
runManyRounds 0 _ s = return s
runManyRounds roundsQuant quant s = do
    hashTable <- runRound quant s
    runManyRounds (roundsQuant - 1) quant hashTable

runRound :: Int -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
runRound quant s = _runRound quant 0 s
  where
    _runRound :: Int -> Int -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
    _runRound 0 idx s = do
        currMonkey <- fromJust <$> H.lookup s idx
        monkeyAction idx (_items currMonkey) s
    _runRound quant idx s = do
        currMonkey <- fromJust <$> H.lookup s idx
        newHash <- monkeyAction idx (_items currMonkey) s
        _runRound (quant - 1) (idx + 1) newHash

day11 :: IO ()
day11 = do
    new_hashtable <- H.new
    txt <- readFile "task_11.txt"
    tst <- runStateT (runParserT (skipMany monkeyParse <* eof) "" txt) (new_hashtable, -1)
    case tst of
        (Left err, s) -> error "Error: parsing of input has failed"
        (Right xs, (s, max_monkey)) -> do
            print s
    let (_, (s, max_monkey)) = tst

    monkeyHashTable <- runManyRounds 20 max_monkey s
    -- currMonkey <- fromJust <$> H.lookup s 0
    -- monkeyHashTable <- monkeyAction 0 (_items currMonkey) s

    currMonkey <- fromJust <$> H.lookup monkeyHashTable 0
    print currMonkey

    currMonkey <- fromJust <$> H.lookup monkeyHashTable 1
    print currMonkey

    currMonkey <- fromJust <$> H.lookup monkeyHashTable 2
    print currMonkey

    checkMonkey <- fromJust <$> H.lookup monkeyHashTable 3
    print checkMonkey

    print "yay"