{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use gets" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

-- I use ParsecT monad transformer here in order to ahve an example of using Parsec subparser
-- inside of ParsecT parser.

module Day12 (day12) where

day12 :: IO ()
day12 = do
    print "yay!"