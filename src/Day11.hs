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
    try,
    (<|>),
 )

import Control.Monad.ST (ST)

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
import Text.Megaparsec.Char.Lexer as L

type HashTable k v = H.BasicHashTable k v
type Parser = ParsecT Void String (StateT (HashTable Integer [Integer]) IO)
type ParserInner = Parsec Void String

monkeyParse :: Parser [Integer]
monkeyParse =
    do
        ht <- get
        string "Monkey "
        monkey_index <- L.decimal
        string ":"
        newline
        state <- getParserState
        -- lst <- itemsParse
        -- lift . lift $ H.insert ht monkey_index lst
        -- lift . lift $ fromJust <$> H.lookup ht monkey_index
        let tpl = runParser' itemsParse state
        case tpl of
            (_, Left err) -> error "Error while parsing"
            (state, Right lst) -> do
                setParserState state
                lift . lift $ H.insert ht monkey_index lst
                lift . lift $ fromJust <$> H.lookup ht monkey_index

itemsParse :: ParserInner [Integer]
itemsParse =
    do
        string "Starting items: "
        many (L.decimal <* (string ", " <|> many newline))

str :: String
str = "Monkey 101:\nStarting items: 1, 2, 3, 4\nMonkey 101:\nStarting items: 5, 6, 7, 8\n"

day11 :: IO ()
day11 = do
    new_hashtable <- H.new
    tst <- runStateT (runParserT (many monkeyParse) "" str) new_hashtable --
    case tst of
        (Left err, s) -> print "Error: parsing of input has failed"
        (Right xs, s) -> print xs
    print "yay"