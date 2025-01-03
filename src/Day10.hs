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

import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Instruction = Noop | Addx Int deriving (Show, Eq, Ord)
type Parser = Parsec Void String

oneLineSequence :: Parser Instruction
oneLineSequence = do
    (Noop <$ (string "noop" <* many newline)) <|> Addx
        <$> ((string "addx ") *> (signed (return ()) decimal) <* many newline)

day10 :: IO ()
day10 = do
    inputs <- fromJust <$> parseMaybe (many oneLineSequence <* eof) <$> (readFile "./task_10.txt")
    print inputs