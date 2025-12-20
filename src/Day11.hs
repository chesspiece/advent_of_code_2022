{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

-- I use ParsecT monad transformer here in order to ahve an example of using Parsec subparser
-- inside of ParsecT parser.

module Day11 (day11) where

import Control.Applicative (many, (*>), (<*))
import Control.Monad (forM_)
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
type Parser = ParsecT Void String (StateT (HashTable Int MonkeyState, Int, Int) IO)
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
    (ht, cnt, modVal) <- get
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
    put (ht, cnt + 1, modVal * divis)
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
                Add     <$> try (string "+ " *> decimal)
            <|> Mult    <$> try (string "* " *> decimal)
            <|> AddOld  <$  try (string "+ old")
            <|> MultOld <$  string "* old"
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

monkeyAction ::
    Int -> Int -> Int -> [Int] -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
monkeyAction dv md currMonkeyIdx [] monkeyTable = return monkeyTable
monkeyAction dv md currMonkeyIdx worryList monkeyTable = do
    currentMonkeyState <- fromJust <$> H.lookup monkeyTable currMonkeyIdx
    H.insert
        monkeyTable
        currMonkeyIdx
        ( currentMonkeyState
            & items .~ tail worryList
            & inspectsQuantity .~ _inspectsQuantity currentMonkeyState + 1
        )

    let newWorry = mod (div (applyMonkeyOperation (_opertion currentMonkeyState) (head worryList)) dv) md
    let checkDone = isDivisible newWorry (_divisibilityCheck currentMonkeyState)
    let newMonkeyIdx = chooseMonkey (_monkeyChoice currentMonkeyState) checkDone

    nextMonkey <- fromJust <$> H.lookup monkeyTable newMonkeyIdx

    let newMonkeyItems = _items nextMonkey
    let newMonkeyState = nextMonkey & (items .~ (newMonkeyItems ++ [newWorry]))

    H.insert monkeyTable newMonkeyIdx newMonkeyState

    currentMonkeyState <- fromJust <$> H.lookup monkeyTable currMonkeyIdx

    monkeyAction dv md currMonkeyIdx (_items currentMonkeyState) monkeyTable

runManyRounds ::
    Int -> Int -> Int -> Int -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
runManyRounds dv md 0 _ monkeyHashTable = return monkeyHashTable
runManyRounds dv md roundsQuant quant monkeyHashTable = do
    hashTable <- runRound dv md quant monkeyHashTable
    runManyRounds dv md (roundsQuant - 1) quant hashTable

runRound :: Int -> Int -> Int -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
runRound dv md quant s = _runRound dv md quant 0 s
  where
    _runRound :: Int -> Int -> Int -> Int -> HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
    _runRound dv md 0 idx s = do
        currMonkey <- fromJust <$> H.lookup s idx
        monkeyAction dv md idx (_items currMonkey) s
    _runRound dv md quant idx s = do
        currMonkey <- fromJust <$> H.lookup s idx
        newHash <- monkeyAction dv md idx (_items currMonkey) s
        _runRound dv md (quant - 1) (idx + 1) newHash

findTwoMaxMult :: Int -> HashTable Int MonkeyState -> IO Int
findTwoMaxMult quant = _findTwoMaxMult quant 0 0
  where
    _findTwoMaxMult :: Int -> Int -> Int -> HashTable Int MonkeyState -> IO Int
    _findTwoMaxMult 0 max1 max2 mokeyHashTable = do
        currMonkey <- fromJust <$> H.lookup mokeyHashTable 0
        let quantity = _inspectsQuantity currMonkey
        if quantity > max1
            then return $ quantity * max1
        else if quantity > max2
            then return $ quantity * max1
        else
            return $ max1 * max2
    _findTwoMaxMult idx max1 max2 monkeyHashTable = do
        currMonkey <- fromJust <$> H.lookup monkeyHashTable idx
        let quantity = _inspectsQuantity currMonkey
        if quantity > max1
            then _findTwoMaxMult (idx - 1) quantity max1 monkeyHashTable
        else if quantity > max2
            then _findTwoMaxMult (idx - 1) max1 quantity monkeyHashTable
        else
            _findTwoMaxMult (idx - 1) max1 max2 monkeyHashTable

copyHashTable :: HashTable Int MonkeyState -> IO (HashTable Int MonkeyState)
copyHashTable oldTable = do
    newTable <- H.new
    entries <- H.toList oldTable
    forM_ entries $ \(k, v) -> H.insert newTable k v
    return newTable

day11 :: IO ()
day11 = do
    new_hashtable <- H.new
    txt <- readFile "./inputs/day11.txt"
    tst <- runStateT (runParserT (skipMany monkeyParse <* eof) "" txt) (new_hashtable, -1, 1)
    case tst of
        (Left err, s) -> error "Error: parsing of input has failed"
        (Right xs, (s, max_monkey, modVal)) -> do
            return ()
    let (_, (tablePartOne, max_monkey, modVal)) = tst

    -- print modVal
    tablePartTwo <- copyHashTable tablePartOne

    monkeyHashTable <- runManyRounds 3 modVal 20 max_monkey tablePartOne
    res <- findTwoMaxMult max_monkey monkeyHashTable
    print res

    monkeyHashTable <- runManyRounds 1 modVal 10000 max_monkey tablePartTwo
    res <- findTwoMaxMult max_monkey monkeyHashTable
    print res