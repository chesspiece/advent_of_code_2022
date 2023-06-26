module Main where

import Data.List.Split (splitOn)

parseString :: String -> [[String]]
parseString = map (concatMap (splitOn "-") . splitOn ",") . lines

makeTuple :: [String] -> Maybe (Int, Int, Int, Int)
makeTuple (a : b : c : d : _) = Just (read a, read b, read c, read d)
makeTuple _ = Nothing

check1 :: (Int, Int, Int, Int) -> Int
check1 (a, b, c, d)
  | a <= c && b >= d = 1
  | a >= c && b <= d = 1
  | otherwise = 0

check2 :: (Int, Int, Int, Int) -> Int
check2 (a, b, c, d)
  | b < c || d < a = 0
  | otherwise = 1

main :: IO ()
main = do
  file_str <- readFile "task_4.txt"
  print $
    sum
      (maybe [] (map check1) (mapM makeTuple . parseString $ file_str))
  print $
    sum
      (maybe [] (map check2) (mapM makeTuple . parseString $ file_str))
