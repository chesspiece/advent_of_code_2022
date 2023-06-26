module Main where

import Data.List.Split (splitOn)

parseString :: String -> [[String]]
parseString = map (splitOn " ") . lines

checkString :: [String] -> Int
checkString ["A", "X"] = 1 + 3
checkString ["A", "Y"] = 2 + 6
checkString ["A", "Z"] = 3 + 0
checkString ["B", "X"] = 1 + 0
checkString ["B", "Y"] = 2 + 3
checkString ["B", "Z"] = 3 + 6
checkString ["C", "X"] = 1 + 6
checkString ["C", "Y"] = 2 + 0
checkString ["C", "Z"] = 3 + 3
checkString _ = -1

checkStringV2 :: [String] -> Int
checkStringV2 ["A", "X"] = 3 + 0
checkStringV2 ["A", "Y"] = 1 + 3
checkStringV2 ["A", "Z"] = 2 + 6
checkStringV2 ["B", "X"] = 1 + 0
checkStringV2 ["B", "Y"] = 2 + 3
checkStringV2 ["B", "Z"] = 3 + 6
checkStringV2 ["C", "X"] = 2 + 0
checkStringV2 ["C", "Y"] = 3 + 3
checkStringV2 ["C", "Z"] = 1 + 6
checkStringV2 _ = -1

main :: IO ()
main = do
  file_str <- readFile "task_2.txt"
  print $ sum . map checkString . parseString $ file_str
  print $ sum . map checkStringV2 . parseString $ file_str
