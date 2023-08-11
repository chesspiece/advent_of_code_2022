{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module Day6 (day6) where

parseInputString :: Int -> String -> Int
parseInputString marker_condition = parseInputStringHelper marker_condition (-marker_condition) 0 ""
  where
    parseInputStringHelper :: Int -> Int -> Int -> String -> String -> Int
    parseInputStringHelper _ 0 acc _ _ = acc
    parseInputStringHelper marker_condition chk acc check_list (c : str) = case c `elem` check_list of
      True -> parseInputStringHelper marker_condition (length new_check_list - marker_condition) (acc + 1) new_check_list str
      False -> parseInputStringHelper marker_condition (chk + 1) (acc + 1) (check_list ++ [c]) str
      where
        new_check_list = tail . dropWhile (/= c) $ check_list ++ [c]

day6 :: IO ()
day6 = do
  input_stream <- readFile "./task_6.txt"
  print $ parseInputString 4 input_stream
  print $ parseInputString 14 input_stream
