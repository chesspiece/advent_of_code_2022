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
    Stream (Tokens),
    eof,
    getInput,
    option,
    optional,
    parseMaybe,
    runParser,
    runParser',
    runParserT,
    runParserT',
    setParserState,
    skipMany,
    try,
    (<|>),
 )

import Control.Lens (Identity (runIdentity), makeLenses, (&), (.~))
import Control.Monad (liftM)
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

import Control.Arrow (Arrow (second))
import Data.Either (fromRight)
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust, fromMaybe)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, string)
import qualified Text.Megaparsec.Char as CH
import Text.Megaparsec.Char.Lexer as L (decimal)

data Operation where
    Mult :: Int -> Operation
    Add :: Int -> Operation
    AddOld :: Operation
    MultOld :: Operation
    deriving (Show, Eq, Ord)

type HashTable k v = H.BasicHashTable k v
type Parser = ParsecT Void String (StateT (HashTable Int TaskState) IO)
type ParserInner = Parsec Void String

data TaskState = TaskState
    { _items :: [Int]
    , _divisibilityCheck :: Int
    , _opertion :: Operation
    , _monkeyChoice :: (Int, Int)
    }
    deriving (Show)

makeLenses ''TaskState

monkeyParse :: Parser ()
monkeyParse = do
    ht <- get
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
            TaskState
                { _items = list_items
                , _divisibilityCheck = divis
                , _opertion = opr
                , _monkeyChoice = (monkey1, monkey2)
                }
    lift . lift $ H.insert ht monkey_index parsed_state
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
                <$ (string "* old")
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
        newline
        return (ret, true_val, false_val)

day11 :: IO ()
day11 = do
    new_hashtable <- H.new
    txt <- (readFile "task_11.txt")
    tst <- runStateT (runParserT (skipMany monkeyParse <* eof) "" txt) new_hashtable
    case tst of
        (Left err, s) -> print "Error: parsing of input has failed"
        (Right xs, s) -> do
            print s
            res1 <- H.lookup s 0
            print res1
            res1 <- H.lookup s 1
            print res1
            res1 <- H.lookup s 7
            print res1
            res1 <- H.lookup s 6
            print res1
            res1 <- H.lookup s 8
            print res1
    print "yay"