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
    MonadParsec (getParserState),
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
type Parser = ParsecT Void String (StateT (HashTable Int [Int]) IO)
type ParserInner = Parsec Void String

monkeyParse :: Parser ()
monkeyParse = do
    ht <- get
    string "Monkey "
    monkey_index <- L.decimal
    string ":"
    newline
    state <- getParserState
    let tpl = runParser' itemsParse state
    case tpl of
        (_, Left err) -> error "Parse error!"
        (_, Right lst) -> do
            lift . lift $ H.insert ht monkey_index lst
            put ht
    let (state, _) = tpl
    let tpl = runParser' operationParse state
    case tpl of
        (_, Left err) -> error "Parse error! 2"
        (state, Right lst) -> do
            setParserState state
    many newline
    return ()

itemsParse :: ParserInner [Int]
itemsParse =
    do
        string "Starting items: "
        many (L.decimal <* (string ", " <|> string "\n"))

operationParse :: ParserInner Operation
operationParse =
    do
        string "Operation: new = old "
        ret <-
            Add <$> try (string "+ " *> decimal) <|>
            Mult <$> try (string "* " *> decimal) <|>
            AddOld <$ try (string "+ old") <|>
            MultOld <$ (string "* old")
        newline
        return ret

divisibleParse :: ParserInner Int
divisibleParse =
    do
        string "Test: divisible by "
        ret <- L.decimal
        newline
        return ret

day11 :: IO ()
day11 = do
    new_hashtable <- H.new
    txt <- (readFile "task_11.txt")
    tst <- runStateT (runParserT (skipMany monkeyParse <* eof) "" txt) new_hashtable
    case tst of
        (Left err, s) -> print "Error: parsing of input has failed"
        (Right xs, s) -> do
            print s
            res1 <- H.lookup s 102
            print res1
            res1 <- H.lookup s 1
            print res1
            print xs
    print "yay"