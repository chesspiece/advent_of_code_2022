{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day11 (day11) where

import Control.Applicative (many, (*>), (<*))
import Control.Monad.Trans (lift)
import Text.Megaparsec (
    MonadParsec,
    Parsec,
    ParsecT,
    Stream (Tokens),
    eof,
    getInput,
    option,
    optional,
    parseMaybe,
    runParser,
    runParserT,
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

import Control.Arrow (Arrow (second))
import qualified Data.HashTable.IO as H
import Data.Maybe (fromJust, fromMaybe)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer as L

type HashTable k v = H.BasicHashTable k v
type Parser = Parsec Void String

data TaskState = TaskState
    { _timerCount :: Integer
    , _regCount :: Integer
    }
    deriving (Show)

monkeyParse :: Parser (Integer, [Integer])
monkeyParse =
    do
        string "Monkey "
        monkey_index <- L.decimal
        string ":"
        newline
        items <- itemsParse
        return (monkey_index, items)

itemsParse :: Parser [Integer]
itemsParse =
    do
        string "Starting items: "
        many (L.decimal <* (string ", " <|> many newline))

str :: String
str = "Monkey 101:\nStarting items: 1, 2, 3, 4\n"

day11 :: IO ()
day11 = do
    -- new_hashtable <- H.new
    let tst = parseMaybe monkeyParse str
    case tst of
        Nothing -> print "Error: parsing of input has failed"
        Just xs -> print xs
    print "yay"
