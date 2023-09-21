{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use camelCase" #-}
module Day8 (day8) where

import Control.Monad.ST
import Data.Char (digitToInt)
import Control.Lens

firstRun :: [[Int]] -> [[Int]]
firstRun = undefined

secondRun :: [[Int]] -> [[Int]]
secondRun = undefined

thirdRun :: [[Int]] -> [[Bool]]
thirdRun = undefined

day8 :: IO ()
day8 = do
  input_stream <- readFile "./task_8.txt" >>= return . map (map digitToInt) . lines
  print input_stream
