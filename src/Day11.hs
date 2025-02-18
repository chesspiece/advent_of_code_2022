-- I use ParsecT monad transformer here in order to ahve an example of using Parsec subparser
-- inside of ParsecT parser.
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
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

-- (decimal, signed)

import Control.Arrow (Arrow (second))
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust, fromMaybe)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, string)
import qualified Text.Megaparsec.Char as CH
import Text.Megaparsec.Char.Lexer as L

data Operation = Mult String | Add String
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
        (state, Right lst) -> do
            setParserState state
            lift . lift $ H.insert ht monkey_index lst
            put ht
    state <- getParserState
    let tpl = runParser' operationParse state
    case tpl of
        (_, Left err) -> error "Parse error! 2"
        (state, Right lst) -> do
            setParserState state
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
        ret <- Add <$> (string "+ " *> many CH.printChar) <|> Mult <$> (string "* " *> many CH.printChar)
        newline
        return ret

str :: String
str =
    "Monkey 101:\nStarting items: 1, 2, 3, 4\nOperation: new = old + 2\nMonkey 102:\nStarting items: 5, 6, 7, 8\nOperation: new = old * old\n"

day11 :: IO ()
day11 = do
    new_hashtable <- H.new
    tst <- runStateT (runParserT (skipMany monkeyParse <* eof) "" str) new_hashtable
    case tst of
        (Left err, s) -> print "Error: parsing of input has failed"
        (Right xs, s) -> do
            print s
            res1 <- H.lookup s 102
            print res1
            res1 <- H.lookup s 101
            print res1
            print xs
    print "yay"