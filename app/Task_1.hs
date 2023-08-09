module Task_1 where

import Data.List (sort)
import Data.List.Split (splitOn)

parseString :: String -> [[Int]]
parseString = map (map read . lines) . splitOn "\n\n"

computeAnsw :: [[Int]] -> Int -> [Int]
computeAnsw takeList quant = take quant . reverse . sort . map sum $ takeList

task :: IO ()
task = do
  -- print "yay"
  file_str <- readFile "task_1.txt"
  print $ head . computeAnsw (parseString file_str) $ 1
  print $ sum . computeAnsw (parseString file_str) $ 3
