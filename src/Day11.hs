{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
module Day11 (day11) where

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
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

monkeyParse :: Parser Int
monkeyParse = string "Monkey " *> decimal <* string ":" <* newline

day11 :: IO ()
day11 = do
    print "yay"
